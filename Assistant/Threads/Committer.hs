{- git-annex assistant commit thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Assistant.Threads.Committer where

import Assistant.Common
import Assistant.Changes
import Assistant.Types.Changes
import Assistant.Commits
import Assistant.Alert
import Assistant.DaemonStatus
import Assistant.Threads.Watcher
import Assistant.TransferQueue
import Logs.Transfer
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.HashObject
import qualified Git.LsFiles
import qualified Git.Version
import Git.Types
import qualified Command.Add
import Utility.ThreadScheduler
import qualified Utility.Lsof as Lsof
import qualified Utility.DirWatcher as DirWatcher
import Types.KeySource
import Config
import Annex.Exception
import Annex.Content
import qualified Annex

import Data.Time.Clock
import Data.Tuple.Utils
import qualified Data.Set as S
import Data.Either

{- This thread makes git commits at appropriate times. -}
commitThread :: NamedThread
commitThread = NamedThread "Committer" $ do
	delayadd <- liftAnnex $
		maybe delayaddDefault (return . Just . Seconds)
			=<< annexDelayAdd <$> Annex.getGitConfig
	runEvery (Seconds 1) <~> do
		-- We already waited one second as a simple rate limiter.
		-- Next, wait until at least one change is available for
		-- processing.
		changes <- getChanges
		-- Now see if now's a good time to commit.
		time <- liftIO getCurrentTime
		if shouldCommit time changes
			then do
				readychanges <- handleAdds delayadd changes
				if shouldCommit time readychanges
					then do
						debug
							[ "committing"
							, show (length readychanges)
							, "changes"
							]
						void $ alertWhile commitAlert $
							liftAnnex commitStaged
						recordCommit
					else refill readychanges
			else refill changes
  where
	refill [] = noop
	refill cs = do
		debug ["delaying commit of", show (length cs), "changes"]
		refillChanges cs

commitStaged :: Annex Bool
commitStaged = do
	{- This could fail if there's another commit being made by
	 - something else. -}
	v <- tryAnnex Annex.Queue.flush
	case v of
		Left _ -> return False
		Right _ -> do
			void $ inRepo $ Git.Command.runBool "commit" $ nomessage
				[ Param "--quiet"
				{- Avoid running the usual git-annex pre-commit hook;
				 - watch does the same symlink fixing, and we don't want
				 - to deal with unlocked files in these commits. -}
				, Param "--no-verify"
				]
			{- Empty commits may be made if tree changes cancel
			 - each other out, etc. Git returns nonzero on those,
			 - so don't propigate out commit failures. -}
			return True
  where
	nomessage ps
		| Git.Version.older "1.7.2" = Param "-m"
			: Param "autocommit" : ps
		| otherwise = Param "--allow-empty-message" 
			: Param "-m" : Param "" : ps

{- Decide if now is a good time to make a commit.
 - Note that the list of change times has an undefined order.
 -
 - Current strategy: If there have been 10 changes within the past second,
 - a batch activity is taking place, so wait for later.
 -}
shouldCommit :: UTCTime -> [Change] -> Bool
shouldCommit now changes
	| len == 0 = False
	| len > 10000 = True -- avoid bloating queue too much
	| length (filter thisSecond changes) < 10 = True
	| otherwise = False -- batch activity
  where
	len = length changes
	thisSecond c = now `diffUTCTime` changeTime c <= 1

{- OSX needs a short delay after a file is added before locking it down,
 - when using a non-direct mode repository, as pasting a file seems to
 - try to set file permissions or otherwise access the file after closing
 - it. -}
delayaddDefault :: Annex (Maybe Seconds)
#ifdef darwin_HOST_OS
delayaddDefault = ifM isDirect
	( return Nothing
	, return $ Just $ Seconds 1
	)
#else
delayaddDefault = return Nothing
#endif

{- If there are PendingAddChanges, or InProcessAddChanges, the files
 - have not yet actually been added to the annex, and that has to be done
 - now, before committing.
 -
 - Deferring the adds to this point causes batches to be bundled together,
 - which allows faster checking with lsof that the files are not still open
 - for write by some other process, and faster checking with git-ls-files
 - that the files are not already checked into git.
 -
 - When a file is added, Inotify will notice the new symlink. So this waits
 - for additional Changes to arrive, so that the symlink has hopefully been
 - staged before returning, and will be committed immediately.
 -
 - OTOH, for kqueue, eventsCoalesce, so instead the symlink is directly
 - created and staged.
 -
 - Returns a list of all changes that are ready to be committed.
 - Any pending adds that are not ready yet are put back into the ChangeChan,
 - where they will be retried later.
 -}
handleAdds :: Maybe Seconds -> [Change] -> Assistant [Change]
handleAdds delayadd cs = returnWhen (null incomplete) $ do
	let (pending, inprocess) = partition isPendingAddChange incomplete
	direct <- liftAnnex isDirect
	pending' <- if direct
		then return pending
		else findnew pending
	(postponed, toadd) <- partitionEithers <$> safeToAdd delayadd pending' inprocess

	unless (null postponed) $
		refillChanges postponed

	returnWhen (null toadd) $ do
		added <- catMaybes <$> forM toadd add
		if DirWatcher.eventsCoalesce || null added || direct
			then return $ added ++ otherchanges
			else do
				r <- handleAdds delayadd =<< getChanges
				return $ r ++ added ++ otherchanges
  where
	(incomplete, otherchanges) = partition (\c -> isPendingAddChange c || isInProcessAddChange c) cs
		
	findnew [] = return []
	findnew pending@(exemplar:_) = do
		(!newfiles, cleanup) <- liftAnnex $
			inRepo (Git.LsFiles.notInRepo False $ map changeFile pending)
		void $ liftIO cleanup
		-- note: timestamp info is lost here
		let ts = changeTime exemplar
		return $ map (PendingAddChange ts) newfiles

	returnWhen c a
		| c = return otherchanges
		| otherwise = a

	add :: Change -> Assistant (Maybe Change)
	add change@(InProcessAddChange { keySource = ks }) = do
		alertWhile' (addFileAlert $ keyFilename ks) $
			liftM ret $ catchMaybeIO <~> do
				sanitycheck ks $ do
					key <- liftAnnex $ do
						showStart "add" $ keyFilename ks
						Command.Add.ingest $ Just ks
					done (finishedChange change) (keyFilename ks) key
	  where
		{- Add errors tend to be transient and will be automatically
		 - dealt with, so don't pass to the alert code. -}
		ret (Just j@(Just _)) = (True, j)
		ret _ = (True, Nothing)
	add _ = return Nothing

	done _ _ Nothing = do
		liftAnnex showEndFail
		return Nothing
	done change file (Just key) = do
		link <- liftAnnex $ ifM isDirect
			( calcGitLink file key
			, Command.Add.link file key True
			)
		liftAnnex $ whenM (pure DirWatcher.eventsCoalesce <||> isDirect) $ do
			sha <- inRepo $
				Git.HashObject.hashObject BlobObject link
			stageSymlink file sha
			showEndOk
		queueTransfers Next key (Just file) Upload
		return $ Just change

	{- Check that the keysource's keyFilename still exists,
	 - and is still a hard link to its contentLocation,
	 - before ingesting it. -}
	sanitycheck keysource a = do
		fs <- liftIO $ getSymbolicLinkStatus $ keyFilename keysource
		ks <- liftIO $ getSymbolicLinkStatus $ contentLocation keysource
		if deviceID ks == deviceID fs && fileID ks == fileID fs
			then a
			else do
				-- remove the hard link
				void $ liftIO $ tryIO $ removeFile $ contentLocation keysource
				return Nothing

{- Files can Either be Right to be added now,
 - or are unsafe, and must be Left for later.
 -
 - Check by running lsof on the temp directory, which
 - the KeySources are locked down in.
 -}
safeToAdd :: Maybe Seconds -> [Change] -> [Change] -> Assistant [Either Change Change]
safeToAdd _ [] [] = return []
safeToAdd delayadd pending inprocess = do
	maybe noop (liftIO . threadDelaySeconds) delayadd
	liftAnnex $ do
		keysources <- mapM Command.Add.lockDown (map changeFile pending)
		let inprocess' = catMaybes $ 
			map mkinprocess (zip pending keysources)
		tmpdir <- fromRepo gitAnnexTmpDir
		openfiles <- S.fromList . map fst3 . filter openwrite <$>
			liftIO (Lsof.queryDir tmpdir)
		let checked = map (check openfiles) $ inprocess ++ inprocess'

		{- If new events are received when files are closed,
		 - there's no need to retry any changes that cannot
		 - be done now. -}
		if DirWatcher.closingTracked
			then do
				mapM_ canceladd $ lefts checked
				allRight $ rights checked
			else return checked
  where
	check openfiles change@(InProcessAddChange { keySource = ks })
		| S.member (contentLocation ks) openfiles = Left change
	check _ change = Right change

	mkinprocess (c, Just ks) = Just $ InProcessAddChange
		{ changeTime = changeTime c
		, keySource = ks
		}
	mkinprocess (_, Nothing) = Nothing

	canceladd (InProcessAddChange { keySource = ks }) = do
		warning $ keyFilename ks
			++ " still has writers, not adding"
		-- remove the hard link
		void $ liftIO $ tryIO $ removeFile $ contentLocation ks
	canceladd _ = noop

	openwrite (_file, mode, _pid) =
		mode == Lsof.OpenWriteOnly || mode == Lsof.OpenReadWrite

	allRight = return . map Right

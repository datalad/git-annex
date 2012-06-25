{- git-annex assistant commit thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.Committer where

import Common.Annex
import Assistant.Changes
import Assistant.Commits
import Assistant.ThreadedMonad
import Assistant.Threads.Watcher
import qualified Annex
import qualified Annex.Queue
import qualified Git.Command
import qualified Git.HashObject
import Git.Types
import qualified Command.Add
import Utility.ThreadScheduler
import qualified Utility.Lsof as Lsof
import qualified Utility.DirWatcher as DirWatcher
import Types.KeySource

import Data.Time.Clock
import Data.Tuple.Utils
import qualified Data.Set as S
import Data.Either

{- This thread makes git commits at appropriate times. -}
commitThread :: ThreadState -> ChangeChan -> CommitChan -> IO ()
commitThread st changechan commitchan = runEvery (Seconds 1) $ do
	-- We already waited one second as a simple rate limiter.
	-- Next, wait until at least one change is available for
	-- processing.
	changes <- getChanges changechan
	-- Now see if now's a good time to commit.
	time <- getCurrentTime
	if shouldCommit time changes
		then do
			readychanges <- handleAdds st changechan changes
			if shouldCommit time readychanges
				then do
					void $ tryIO $ runThreadState st commitStaged
					recordCommit commitchan (Commit time)
				else refillChanges changechan readychanges
		else refillChanges changechan changes

commitStaged :: Annex ()
commitStaged = do
	Annex.Queue.flush
	inRepo $ Git.Command.run "commit"
		[ Param "--allow-empty-message"
		, Param "-m", Param ""
		-- Empty commits may be made if tree changes cancel
		-- each other out, etc
		, Param "--allow-empty"
		-- Avoid running the usual git-annex pre-commit hook;
		-- watch does the same symlink fixing, and we don't want
		-- to deal with unlocked files in these commits.
		, Param "--quiet"
		]

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

{- If there are PendingAddChanges, the files have not yet actually been
 - added to the annex (probably), and that has to be done now, before
 - committing.
 -
 - Deferring the adds to this point causes batches to be bundled together,
 - which allows faster checking with lsof that the files are not still open
 - for write by some other process.
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
handleAdds :: ThreadState -> ChangeChan -> [Change] -> IO [Change]
handleAdds st changechan cs = returnWhen (null pendingadds) $ do
	(postponed, toadd) <- partitionEithers <$>
		safeToAdd st pendingadds

	unless (null postponed) $
		refillChanges changechan postponed

	returnWhen (null toadd) $ do
		added <- catMaybes <$> forM toadd add
		if (DirWatcher.eventsCoalesce || null added)
			then return $ added ++ otherchanges
			else do
				r <- handleAdds st changechan
					=<< getChanges changechan
				return $ r ++ added ++ otherchanges
	where
		(pendingadds, otherchanges) = partition isPendingAddChange cs

		returnWhen c a
			| c = return otherchanges
			| otherwise = a

		add :: Change -> IO (Maybe Change)
		add change@(PendingAddChange { keySource = ks }) = do
			r <- catchMaybeIO $ sanitycheck ks $ runThreadState st $ do
				showStart "add" $ keyFilename ks
				handle (finishedChange change) (keyFilename ks)
					=<< Command.Add.ingest ks
			return $ maybeMaybe r
		add _ = return Nothing

		maybeMaybe (Just j@(Just _)) = j
		maybeMaybe _ = Nothing

		handle _ _ Nothing = do
			showEndFail
			return Nothing
		handle change file (Just key) = do
			link <- Command.Add.link file key True
			when DirWatcher.eventsCoalesce $ do
				sha <- inRepo $
					Git.HashObject.hashObject BlobObject link
				stageSymlink file sha
			showEndOk
			return $ Just change

		{- Check that the keysource's keyFilename still exists,
		 - and is still a hard link to its contentLocation,
		 - before ingesting it. -}
		sanitycheck keysource a = do
			fs <- getSymbolicLinkStatus $ keyFilename keysource
			ks <- getSymbolicLinkStatus $ contentLocation keysource
			if deviceID ks == deviceID fs && fileID ks == fileID fs
				then a
				else return Nothing

{- PendingAddChanges can Either be Right to be added now,
 - or are unsafe, and must be Left for later.
 -
 - Check by running lsof on the temp directory, which
 - the KeySources are locked down in.
 -}
safeToAdd :: ThreadState -> [Change] -> IO [Either Change Change]
safeToAdd st changes = runThreadState st $
	ifM (Annex.getState Annex.force)
		( allRight changes -- force bypasses lsof check
		, do
			tmpdir <- fromRepo gitAnnexTmpDir
			openfiles <- S.fromList . map fst3 . filter openwrite <$>
				liftIO (Lsof.queryDir tmpdir)
			
			let checked = map (check openfiles) changes

			{- If new events are received when files are closed,
			 - there's no need to retry any changes that cannot
			 - be done now. -}
			if DirWatcher.closingTracked
				then do
					mapM_ canceladd $ lefts checked
					allRight $ rights checked
				else return checked
		)
	where
		check openfiles change@(PendingAddChange { keySource = ks })
			| S.member (contentLocation ks) openfiles = Left change
		check _ change = Right change

		canceladd (PendingAddChange { keySource = ks }) = do
			warning $ keyFilename ks
				++ " still has writers, not adding"
			-- remove the hard link
			void $ liftIO $ tryIO $
				removeFile $ contentLocation ks
		canceladd _ = noop

		openwrite (_file, mode, _pid) =
			mode == Lsof.OpenWriteOnly || mode == Lsof.OpenReadWrite

		allRight = return . map Right

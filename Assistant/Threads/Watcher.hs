{- git-annex assistant tree watcher
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, CPP #-}

module Assistant.Threads.Watcher (
	watchThread,
	WatcherControl(..),
	checkCanWatch,
	needLsof,
	onAddSymlink,
	runHandler,
) where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Changes
import Assistant.Types.Changes
import Assistant.Alert
import Utility.DirWatcher
import Utility.DirWatcher.Types
import Utility.InodeCache
import qualified Utility.RawFilePath as R
import qualified Annex
import qualified Annex.Queue
import qualified Git
import qualified Git.UpdateIndex
import qualified Git.LsFiles as LsFiles
import Annex.WorkTree
import Annex.CatFile
import Annex.CheckIgnore
import Annex.Link
import Annex.Content
import Annex.ReplaceFile
import Annex.InodeSentinal
import Git.Types
import Git.FilePath
import Config.GitConfig
import Utility.ThreadScheduler
import Logs.Location
import qualified Database.Keys
#ifndef mingw32_HOST_OS
import qualified Utility.Lsof as Lsof
#endif

import Data.Typeable
import qualified Data.ByteString.Lazy as L
import qualified Control.Exception as E
import Data.Time.Clock
import System.PosixCompat.Files (fileMode, statusChangeTime)

checkCanWatch :: Annex ()
checkCanWatch
	| canWatch = do
#ifndef mingw32_HOST_OS
		liftIO Lsof.setup
		unlessM (liftIO (inSearchPath "lsof") <||> Annex.getRead Annex.force)
			needLsof
#else
		noop
#endif
	| otherwise = giveup "watch mode is not available on this system"

needLsof :: Annex ()
needLsof = giveup $ unlines
	[ "The lsof command is needed for watch mode to be safe, and is not in PATH."
	, "To override lsof checks to ensure that files are not open for writing"
	, "when added to the annex, you can use --force"
	, "Be warned: This can corrupt data in the annex, and make fsck complain."
	]

{- A special exception that can be thrown to pause or resume the watcher. -}
data WatcherControl = PauseWatcher | ResumeWatcher
	deriving (Show, Eq, Typeable)

instance E.Exception WatcherControl

watchThread :: NamedThread
watchThread = namedThread "Watcher" $
	ifM (liftAnnex $ getGitConfigVal annexAutoCommit)
		( runWatcher
		, waitFor ResumeWatcher runWatcher
		)

runWatcher :: Assistant ()
runWatcher = do
	startup <- asIO1 startupScan
	symlinkssupported <- liftAnnex $ coreSymlinks <$> Annex.getGitConfig
	addhook <- hook $ onAddFile symlinkssupported
	delhook <- hook onDel
	addsymlinkhook <- hook onAddSymlink
	deldirhook <- hook onDelDir
	errhook <- hook onErr
	let hooks = mkWatchHooks
		{ addHook = addhook
		, delHook = delhook
		, addSymlinkHook = addsymlinkhook
		, delDirHook = deldirhook
		, errHook = errhook
		}
	scanevents <- liftAnnex $ annexStartupScan <$> Annex.getGitConfig
	h <- liftIO $ watchDir "." ignored scanevents hooks startup
	debug [ "watching", "."]
	
	{- Let the DirWatcher thread run until signalled to pause it,
	 - then wait for a resume signal, and restart. -}
	waitFor PauseWatcher $ do
		liftIO $ stopWatchDir h
		waitFor ResumeWatcher runWatcher
  where
	hook a = Just <$> asIO2 (runHandler a)

waitFor :: WatcherControl -> Assistant () -> Assistant ()
waitFor sig next = do
	r <- liftIO (E.try pause :: IO (Either E.SomeException ()))
	case r of
		Left e -> case E.fromException e of
			Just s
				| s == sig -> next
			_ -> noop
		_ -> noop
  where
	pause = runEvery (Seconds 86400) noop

{- Initial scartup scan. The action should return once the scan is complete. -}
startupScan :: IO a -> Assistant a
startupScan scanner = do
	liftAnnex $ showAction "scanning"
	alertWhile' startupScanAlert $ do
		r <- liftIO scanner

		-- Notice any files that were deleted before
		-- watching was started.
		top <- liftAnnex $ fromRepo Git.repoPath
		(fs, cleanup) <- liftAnnex $ inRepo $ LsFiles.deleted [] [top]
		forM_ fs $ \f -> do
			let f' = fromRawFilePath f
			liftAnnex $ onDel' f'
			maybe noop recordChange =<< madeChange f' RmChange
		void $ liftIO cleanup
		
		liftAnnex $ showAction "started"
		liftIO $ putStrLn ""
		
		modifyDaemonStatus_ $ \s -> s { scanComplete = True }

		-- Ensure that the Committer sees any changes
		-- that it did not process, and acts on them now that
		-- the scan is complete.
		refillChanges =<< getAnyChanges

		return (True, r)

{- Hardcoded ignores, passed to the DirWatcher so it can avoid looking
 - at the entire .git directory. Does not include .gitignores. -}
ignored :: FilePath -> Bool
ignored = ig . takeFileName
  where
	ig ".git" = True
	ig ".gitignore" = True
	ig ".gitattributes" = True
#ifdef darwin_HOST_OS
	ig ".DS_Store" = True
#endif
	ig _ = False

unlessIgnored :: FilePath -> Assistant (Maybe Change) -> Assistant (Maybe Change)
unlessIgnored file a = ifM (liftAnnex $ checkIgnored (CheckGitIgnore True) (toRawFilePath file))
	( noChange
	, a
	)

type Handler = FilePath -> Maybe FileStatus -> Assistant (Maybe Change)

{- Runs an action handler, and if there was a change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: Handler -> FilePath -> Maybe FileStatus -> Assistant ()
runHandler handler file filestatus = void $ do
	r <- tryIO <~> handler (normalize file) filestatus
	case r of
		Left e -> liftAnnex $ warning $ UnquotedString $ show e
		Right Nothing -> noop
		Right (Just change) -> recordChange change
  where
	normalize f
		| "./" `isPrefixOf` file = drop 2 f
		| otherwise = f

shouldRestage :: DaemonStatus -> Bool
shouldRestage ds = scanComplete ds || forceRestage ds

onAddFile :: Bool -> Handler
onAddFile symlinkssupported f fs =
	onAddFile' contentchanged addassociatedfile addlink samefilestatus symlinkssupported f fs
  where
	addassociatedfile key file = 
		Database.Keys.addAssociatedFile key
			=<< inRepo (toTopFilePath (toRawFilePath file))
	samefilestatus key file status = do
		cache <- Database.Keys.getInodeCaches key
		curr <- withTSDelta $ \delta ->
			liftIO $ toInodeCache delta (toRawFilePath file) status
		case (cache, curr) of
			(_, Just c) -> elemInodeCaches c cache
			([], Nothing) -> return True
			_ -> return False
	contentchanged oldkey file = do
		Database.Keys.removeAssociatedFile oldkey
			=<< inRepo (toTopFilePath (toRawFilePath file))
		unlessM (inAnnex oldkey) $
			logStatus oldkey InfoMissing
	addlink file key = do
		mode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus (toRawFilePath file)
		liftAnnex $ stagePointerFile (toRawFilePath file) mode =<< hashPointerFile key
		madeChange file $ LinkChange (Just key)

onAddFile'
	:: (Key -> FilePath -> Annex ())
	-> (Key -> FilePath -> Annex ())
	-> (FilePath -> Key -> Assistant (Maybe Change))
	-> (Key -> FilePath -> FileStatus -> Annex Bool)
	-> Bool
	-> Handler
onAddFile' contentchanged addassociatedfile addlink samefilestatus symlinkssupported file fs = do
	v <- liftAnnex $ catKeyFile (toRawFilePath file)
	case (v, fs) of
		(Just key, Just filestatus) ->
			ifM (liftAnnex $ samefilestatus key file filestatus)
				{- It's possible to get an add event for
				 - an existing file that is not
				 - really modified, but it might have
				 - just been deleted and been put back,
				 - so its annex link is restaged to make sure. -}
				( ifM (shouldRestage <$> getDaemonStatus)
					( addlink file key
					, noChange
					)
				, guardSymlinkStandin (Just key) $ do
					debug ["changed", file]
					liftAnnex $ contentchanged key file
					pendingAddChange file
				)
		_ -> unlessIgnored file $
			guardSymlinkStandin Nothing $ do
				debug ["add", file]
				pendingAddChange file
  where
	{- On a filesystem without symlinks, we'll get changes for regular
	 - files that git uses to stand-in for symlinks. Detect when
	 - this happens, and stage the symlink, rather than annexing the
	 - file. -}
	guardSymlinkStandin mk a
		| symlinkssupported = a
		| otherwise = do
			linktarget <- liftAnnex $ getAnnexLinkTarget $
				toRawFilePath file
			case linktarget of
				Nothing -> a
				Just lt -> do
					case parseLinkTargetOrPointer lt of
						Nothing -> noop
						Just key -> liftAnnex $
							addassociatedfile key file
					onAddSymlink' (Just lt) mk file fs

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
onAddSymlink file filestatus = unlessIgnored file $ do
	linktarget <- liftIO (catchMaybeIO $ R.readSymbolicLink file')
	kv <- liftAnnex (lookupKey file')
	onAddSymlink' linktarget kv file filestatus
  where
	file' = toRawFilePath file

onAddSymlink' :: Maybe LinkTarget -> Maybe Key -> Handler
onAddSymlink' linktarget mk file filestatus = go mk
  where
	go (Just key) = do
		link <- liftAnnex $ calcRepo $ gitAnnexLink (toRawFilePath file) key
		if linktarget == Just link
			then ensurestaged (Just link) =<< getDaemonStatus
			else do
				liftAnnex $ replaceWorkTreeFile file $
					makeAnnexLink link
				addLink file link (Just key)
	-- other symlink, not git-annex
	go Nothing = ensurestaged linktarget =<< getDaemonStatus

	{- This is often called on symlinks that are already
	 - staged correctly. A symlink may have been deleted
	 - and being re-added, or added when the watcher was
	 - not running. So they're normally restaged to make sure.
	 -
	 - As an optimisation, during the startup scan, avoid
	 - restaging everything. Only links that were created since
	 - the last time the daemon was running are staged.
	 - (If the daemon has never ran before, avoid staging
	 - links too.)
	 -}
	ensurestaged (Just link) daemonstatus
		| shouldRestage daemonstatus = addLink file link mk
		| otherwise = case filestatus of
			Just s
				| not (afterLastDaemonRun (statusChangeTime s) daemonstatus) -> noChange
			_ -> addLink file link mk
	ensurestaged Nothing _ = noChange

{- For speed, tries to reuse the existing blob for symlink target. -}
addLink :: FilePath -> LinkTarget -> Maybe Key -> Assistant (Maybe Change)
addLink file link mk = do
	debug ["add symlink", file]
	liftAnnex $ do
		v <- catObjectDetails $ Ref $ encodeBS $ ':':file
		case v of
			Just (currlink, sha, _type)
				| L.fromStrict link == currlink ->
					stageSymlink (toRawFilePath file) sha
			_ -> stageSymlink (toRawFilePath file)
				=<< hashSymlink link
	madeChange file $ LinkChange mk

onDel :: Handler
onDel file _ = do
	debug ["file deleted", file]
	liftAnnex $ onDel' file
	madeChange file RmChange

onDel' :: FilePath -> Annex ()
onDel' file = do
	topfile <- inRepo (toTopFilePath (toRawFilePath file))
	withkey $ flip Database.Keys.removeAssociatedFile topfile
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.unstageFile (toRawFilePath file))
  where
	withkey a = maybe noop a =<< catKeyFile (toRawFilePath file)

{- A directory has been deleted, or moved, so tell git to remove anything
 - that was inside it from its cache. Since it could reappear at any time,
 - use --cached to only delete it from the index.
 -
 - This queues up a lot of RmChanges, which assists the Committer in
 - pairing up renamed files when the directory was renamed. -}
onDelDir :: Handler
onDelDir dir _ = do
	debug ["directory deleted", dir]
	(fs, clean) <- liftAnnex $ inRepo $ LsFiles.deleted [] [toRawFilePath dir]
	let fs' = map fromRawFilePath fs

	liftAnnex $ mapM_ onDel' fs'

	-- Get the events queued up as fast as possible, so the
	-- committer sees them all in one block.
	now <- liftIO getCurrentTime
	recordChanges $ map (\f -> Change now f RmChange) fs'

	void $ liftIO clean
	noChange

{- Called when there's an error with inotify or kqueue. -}
onErr :: Handler
onErr msg _ = do
	liftAnnex $ warning (UnquotedString msg)
	void $ addAlert $ warningAlert "watcher" msg
	noChange

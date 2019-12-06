{- git-annex command
 -
 - Copyright 2010,2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Lock where

import Command
import qualified Annex.Queue
import qualified Annex
import Annex.Content
import Annex.Link
import Annex.InodeSentinal
import Annex.Perms
import Annex.ReplaceFile
import Utility.InodeCache
import qualified Database.Keys
import Annex.Ingest
import Logs.Location
import Git.FilePath
	
cmd :: Command
cmd = withGlobalOptions [jsonOptions, annexedMatchingOptions] $
	command "lock" SectionCommon
		"undo unlock command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = do
	l <- workTreeItems ps
	withFilesInGit (commandAction . (whenAnnexed startNew)) l

startNew :: RawFilePath -> Key -> CommandStart
startNew file key = ifM (isJust <$> isAnnexLink file)
	( stop
	, starting "lock" (mkActionItem (key, file)) $
		go =<< liftIO (isPointerFile file)
	)
  where
	go (Just key')
		| key' == key = cont
		| otherwise = errorModified
	go Nothing = 
		ifM (isUnmodified key (fromRawFilePath file))
			( cont
			, ifM (Annex.getState Annex.force)
				( cont
				, errorModified
				)
			)
	cont = performNew file key

performNew :: RawFilePath -> Key -> CommandPerform
performNew file key = do
	lockdown =<< calcRepo (gitAnnexLocation key)
	addLink (fromRawFilePath file) key
		=<< withTSDelta (liftIO . genInodeCache' file)
	next $ cleanupNew file key
  where
	lockdown obj = do
		ifM (isUnmodified key obj)
			( breakhardlink obj
			, repopulate obj
			)
		whenM (liftIO $ doesFileExist obj) $
			freezeContent obj

	-- It's ok if the file is hard linked to obj, but if some other
	-- associated file is, we need to break that link to lock down obj.
	breakhardlink obj = whenM (catchBoolIO $ (> 1) . linkCount <$> liftIO (getFileStatus obj)) $ do
		mfc <- withTSDelta (liftIO . genInodeCache' file)
		unlessM (sameInodeCache obj (maybeToList mfc)) $ do
			modifyContent obj $ replaceFile obj $ \tmp -> do
				unlessM (checkedCopyFile key obj tmp Nothing) $
					giveup "unable to lock file"
			Database.Keys.storeInodeCaches key [obj]

	-- Try to repopulate obj from an unmodified associated file.
	repopulate obj = modifyContent obj $ do
		g <- Annex.gitRepo
		fs <- map (`fromTopFilePath` g)
			<$> Database.Keys.getAssociatedFiles key
		mfile <- firstM (isUnmodified key) fs
		liftIO $ nukeFile obj
		case mfile of
			Just unmodified ->
				unlessM (checkedCopyFile key unmodified obj Nothing)
					lostcontent
			Nothing -> lostcontent

	lostcontent = logStatus key InfoMissing

cleanupNew :: RawFilePath -> Key -> CommandCleanup
cleanupNew file key = do
	Database.Keys.removeAssociatedFile key =<< inRepo (toTopFilePath (fromRawFilePath file))
	return True

startOld :: RawFilePath -> CommandStart
startOld file = do
	unlessM (Annex.getState Annex.force)
		errorModified
	starting "lock" (ActionItemWorkTreeFile file) $
		performOld file

performOld :: RawFilePath -> CommandPerform
performOld file = do
	Annex.Queue.addCommand "checkout" [Param "--"] [fromRawFilePath file]
	next $ return True

errorModified :: a
errorModified =  giveup "Locking this file would discard any changes you have made to it. Use 'git annex add' to stage your changes. (Or, use --force to override)"

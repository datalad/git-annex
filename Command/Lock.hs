{- git-annex command
 -
 - Copyright 2010,2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Lock where

import Command
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
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (linkCount)
	
cmd :: Command
cmd = withAnnexOptions [jsonOptions, annexedMatchingOptions] $
	command "lock" SectionCommon
		"undo unlock command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = withFilesInGitAnnex ww seeker =<< workTreeItems ww ps
  where
	ww = WarnUnmatchLsFiles "lock"
	seeker = AnnexedFileSeeker
		{ startAction = const start
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

start :: SeekInput -> RawFilePath -> Key -> CommandStart
start si file key = ifM (isJust <$> isAnnexLink file)
	( stop
	, starting "lock" (mkActionItem (key, file)) si $
		go =<< liftIO (isPointerFile file)
	)
  where
	go (Just key')
		| key' == key = cont
		| otherwise = errorModified
	go Nothing = 
		ifM (isUnmodified key file)
			( cont
			, ifM (Annex.getRead Annex.force)
				( cont
				, errorModified
				)
			)
	cont = perform file key

perform :: RawFilePath -> Key -> CommandPerform
perform file key = do
	lockdown =<< calcRepo (gitAnnexLocation key)
	addSymlink file key =<< withTSDelta (liftIO . genInodeCache file)
	next $ return True
  where
	lockdown obj = do
		ifM (isUnmodified key obj)
			( breakhardlink obj
			, repopulate obj
			)
		whenM (liftIO $ R.doesPathExist obj) $
			freezeContent obj

	-- It's ok if the file is hard linked to obj, but if some other
	-- associated file is, we need to break that link to lock down obj.
	breakhardlink obj = whenM (catchBoolIO $ (> 1) . linkCount <$> liftIO (R.getFileStatus obj)) $ do
		mfc <- withTSDelta (liftIO . genInodeCache file)
		unlessM (sameInodeCache obj (maybeToList mfc)) $ do
			modifyContentDir obj $ replaceGitAnnexDirFile obj $ \tmp -> do
				unlessM (checkedCopyFile key obj tmp Nothing) $
					giveup "unable to lock file"
			Database.Keys.storeInodeCaches key [obj]

	-- Try to repopulate obj from an unmodified associated file.
	repopulate obj = modifyContentDir obj $ do
		g <- Annex.gitRepo
		fs <- map (`fromTopFilePath` g)
			<$> Database.Keys.getAssociatedFiles key
		mfile <- firstM (isUnmodified key) fs
		liftIO $ removeWhenExistsWith R.removeLink obj
		case mfile of
			Just unmodified ->
				ifM (checkedCopyFile key unmodified obj Nothing)
					( Database.Keys.storeInodeCaches key [obj]
					, lostcontent
					)
			Nothing -> lostcontent

	lostcontent = logStatus NoLiveUpdate key InfoMissing

errorModified :: a
errorModified =  giveup "Locking this file would discard any changes you have made to it. Use 'git annex add' to stage your changes. (Or, use --force to override)"

{- git-annex command
 -
 - Copyright 2010,2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Lock where

import Common.Annex
import Command
import qualified Annex.Queue
import qualified Annex
import Annex.Version
import Annex.Content
import Annex.Link
import Annex.InodeSentinal
import Utility.InodeCache
import qualified Database.Keys
import qualified Command.Add
	
cmd :: Command
cmd = notDirect $ withGlobalOptions annexedMatchingOptions $
	command "lock" SectionCommon
		"undo unlock command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = ifM versionSupportsUnlockedPointers
	( withFilesInGit (whenAnnexed startNew) ps
	, do
		withFilesUnlocked startOld ps
		withFilesUnlockedToBeCommitted startOld ps
	)

startNew :: FilePath -> Key -> CommandStart
startNew file key = do
	showStart "lock" file
	go =<< isPointerFile file
  where
	go (Just key')
		| key' == key = cont False
		| otherwise = errorModified
	go Nothing = 
		ifM (isUnmodified key file) 
			( cont False
			, ifM (Annex.getState Annex.force)
				( cont True
				, errorModified
				)
			)
	cont = next . performNew file key

performNew :: FilePath -> Key -> Bool -> CommandPerform
performNew file key filemodified = do
	-- If other files use this same key, and are unlocked, 
	-- the annex object file might be hard linked to those files.
	-- It's also possible that the annex object file was
	-- modified while the file was unlocked. 
	--
	-- So, in order to lock the file's content, we need to break all
	-- hard links to the annex object file, and if it's modified,
	-- replace it with a copy of the content of one of the associated
	-- files.
	--
	-- When the file being locked is unmodified, the annex object file
	-- can just be linked to it. (Which might already be the case, but
	-- do it again to be sure.)
	--
	-- When the file being locked is modified, find another associated
	-- file that is unmodified, and copy it to the annex object file.
	-- If there are no unmodified associated files, the content of
	-- the key is lost.
	--
	-- If the filesystem doesn't support hard links, none of this
	-- is a concern.
	obj <- calcRepo (gitAnnexLocation key)

	freezeContent obj
	Command.Add.addLink file key
		=<< withTSDelta (liftIO . genInodeCache file)
	next $ cleanupNew file key

cleanupNew :: FilePath -> Key -> CommandCleanup
cleanupNew file key = do
	Database.Keys.removeAssociatedFile key file
	return True

startOld :: FilePath -> CommandStart
startOld file = do
	showStart "lock" file
	unlessM (Annex.getState Annex.force)
		errorModified
	next $ performOld file

performOld :: FilePath -> CommandPerform
performOld file = do
	Annex.Queue.addCommand "checkout" [Param "--"] [file]
	next $ return True

errorModified :: a
errorModified =  error "Locking this file would discard any changes you have made to it. Use 'git annex add' to stage your changes. (Or, use --force to override)"

{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import Control.Monad.State (liftIO)
import System.Directory hiding (copyFile)

import Command
import qualified Annex
import Types
import Messages
import Locations
import Content
import Utility.CopyFile
import Utility

command :: [Command]
command =
	[ repoCommand "unlock" paramPath seek "unlock files for modification"
	, repoCommand "edit" paramPath seek "same as unlock"
	]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- The unlock subcommand replaces the symlink with a copy of the file's
 - content. -}
start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	showStart "unlock" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform dest key = do
	unlessM (inAnnex key) $ error "content not present"
	
	checkDiskSpace key

	g <- Annex.gitRepo
	let src = gitAnnexLocation g key
	let tmpdest = gitAnnexTmpLocation g key
	liftIO $ createDirectoryIfMissing True (parentDir tmpdest)
	showAction "copying"
	ok <- liftIO $ copyFile src tmpdest
        if ok
                then do
			liftIO $ do
				removeFile dest
				renameFile tmpdest dest
				allowWrite dest
			next $ return True
                else error "copy failed!"

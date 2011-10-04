{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import AnnexCommon
import Command
import Content
import Utility.CopyFile
import Utility.FileMode

command :: [Command]
command =
	[ repoCommand "unlock" paramPaths seek "unlock files for modification"
	, repoCommand "edit" paramPaths seek "same as unlock"
	]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- The unlock subcommand replaces the symlink with a copy of the file's
 - content. -}
start :: FilePath -> CommandStart
start file = isAnnexed file $ \(key, _) -> do
	showStart "unlock" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform dest key = do
	unlessM (inAnnex key) $ error "content not present"
	
	checkDiskSpace key

	g <- gitRepo
	let src = gitAnnexLocation g key
	let tmpdest = gitAnnexTmpLocation g key
	liftIO $ createDirectoryIfMissing True (parentDir tmpdest)
	showAction "copying"
	ok <- liftIO $ copyFileExternal src tmpdest
        if ok
                then do
			liftIO $ do
				removeFile dest
				renameFile tmpdest dest
				allowWrite dest
			next $ return True
                else error "copy failed!"

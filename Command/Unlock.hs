{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import Common.Annex
import Command
import Annex.Content
import Utility.CopyFile
import Utility.FileMode

def :: [Command]
def =
	[ c "unlock" "unlock files for modification"
	, c "edit" "same as unlock"
	]
	where
		c n = command n paramPaths seek

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

	src <- fromRepo $ gitAnnexLocation key
	tmpdest <- fromRepo $ gitAnnexTmpLocation key
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

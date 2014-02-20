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

def :: [Command]
def =
	[ c "unlock" "unlock files for modification"
	, c "edit" "same as unlock"
	]
  where
	c n = notDirect . command n paramPaths seek SectionCommon

seek :: CommandSeek
seek = withFilesInGit $ whenAnnexed start

{- The unlock subcommand replaces the symlink with a copy of the file's
 - content. -}
start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = do
	showStart "unlock" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform dest key = do
	unlessM (inAnnex key) $ error "content not present"
	unlessM (checkDiskSpace Nothing key 0) $ error "cannot unlock"

	src <- calcRepo $ gitAnnexLocation key
	tmpdest <- fromRepo $ gitAnnexTmpLocation key
	liftIO $ createDirectoryIfMissing True (parentDir tmpdest)
	showAction "copying"
	ifM (liftIO $ copyFileExternal src tmpdest)
		( do
			liftIO $ do
				removeFile dest
				moveFile tmpdest dest
			thawContent dest
			next $ return True
		, error "copy failed!"
		)

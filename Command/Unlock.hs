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
start :: FilePath -> Key -> CommandStart
start file key = do
	showStart "unlock" file
	ifM (inAnnex key)
		( ifM (checkDiskSpace Nothing key 0)
			( next $ perform file key
			, do
				warning "not enough disk space to copy file"
				next $ next $ return False
			)
		, do
			warning "content not present; cannot unlock"
			next $ next $ return False
		)

perform :: FilePath -> Key -> CommandPerform
perform dest key = do
	src <- calcRepo $ gitAnnexLocation key
	tmpdest <- fromRepo $ gitAnnexTmpObjectLocation key
	liftIO $ createDirectoryIfMissing True (parentDir tmpdest)
	showAction "copying"
	ifM (liftIO $ copyFileExternal src tmpdest)
		( do
			liftIO $ do
				removeFile dest
				moveFile tmpdest dest
			thawContent dest
			next $ return True
		, do
			warning "copy failed!"
			next $ return False
		)

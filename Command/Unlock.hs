{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import Common.Annex
import Command
import Annex.Content
import Annex.CatFile
import Utility.CopyFile

cmd :: Command
cmd = mkcmd "unlock" "unlock files for modification"

editcmd :: Command
editcmd = mkcmd "edit" "same as unlock"

mkcmd :: String -> String -> Command
mkcmd n d = notDirect $ withGlobalOptions annexedMatchingOptions $
	command n SectionCommon d paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withFilesInGit $ whenAnnexed start

{- The unlock subcommand replaces the symlink with a copy of the file's
 - content. -}
start :: FilePath -> Key -> CommandStart
start file key = do
	showStart "unlock" file
	ifM (inAnnex key)
		( ifM (isJust <$> catKeyFileHEAD file)
			( next $ perform file key
			, do
				warning "this has not yet been committed to git; cannot unlock it"
				next $ next $ return False
			)
		, do
			warning "content not present; cannot unlock"
			next $ next $ return False
		)

perform :: FilePath -> Key -> CommandPerform
perform dest key = ifM (checkDiskSpace Nothing key 0 True)
	( do
		src <- calcRepo $ gitAnnexLocation key
		tmpdest <- fromRepo $ gitAnnexTmpObjectLocation key
		liftIO $ createDirectoryIfMissing True (parentDir tmpdest)
		showAction "copying"
		ifM (liftIO $ copyFileExternal CopyAllMetaData src tmpdest)
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
	, do
		warning "not enough disk space to copy file"
		next $ return False
	)

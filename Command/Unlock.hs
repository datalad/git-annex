{- git-annex command
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unlock where

import Command
import Annex.Content
import Annex.Perms
import Annex.CatFile
import Annex.Version
import Annex.Link
import Annex.ReplaceFile
import Utility.CopyFile
import Utility.FileMode
import Git.FilePath
import qualified Database.Keys

cmd :: Command
cmd = mkcmd "unlock" "unlock files for modification"

editcmd :: Command
editcmd = mkcmd "edit" "same as unlock"

mkcmd :: String -> String -> Command
mkcmd n d = notDirect $ withGlobalOptions (jsonOption : annexedMatchingOptions) $
	command n SectionCommon d paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = withFilesInGit (whenAnnexed start) =<< workTreeItems ps

{- Before v6, the unlock subcommand replaces the symlink with a copy of
 - the file's content. In v6 and above, it converts the file from a symlink
 - to a pointer. -}
start :: FilePath -> Key -> CommandStart
start file key = ifM (isJust <$> isAnnexLink file)
	( do
		showStart "unlock" file
		ifM versionSupportsUnlockedPointers
			( next $ performNew file key
			, startOld file key
			)
	, stop
	)

performNew :: FilePath -> Key -> CommandPerform
performNew dest key = do
	destmode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus dest
	replaceFile dest $ \tmp ->
		ifM (inAnnex key)
			( do
				r <- linkFromAnnex key tmp destmode
				case r of
					LinkAnnexOk -> return ()
					LinkAnnexNoop -> return ()
					LinkAnnexFailed -> error "unlock failed"
			, liftIO $ writePointerFile tmp key destmode
			)
	next $ cleanupNew dest key destmode

cleanupNew ::  FilePath -> Key -> Maybe FileMode -> CommandCleanup
cleanupNew dest key destmode = do
	stagePointerFile dest destmode =<< hashPointerFile key
	Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath dest)
	return True

startOld :: FilePath -> Key -> CommandStart
startOld file key = 
	ifM (inAnnex key)
		( ifM (isJust <$> catKeyFileHEAD file)
			( next $ performOld file key
			, do
				warning "this has not yet been committed to git; cannot unlock it"
				next $ next $ return False
			)
		, do
			warning "content not present; cannot unlock"
			next $ next $ return False
		)

performOld :: FilePath -> Key -> CommandPerform
performOld dest key = ifM (checkDiskSpace Nothing key 0 True)
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

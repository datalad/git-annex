{- git-annex command
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Unlock where

import Command
import Annex.Content
import Annex.Perms
import Annex.Link
import Annex.ReplaceFile
import Git.FilePath
import qualified Database.Keys
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = mkcmd "unlock" "unlock files for modification"

editcmd :: Command
editcmd = mkcmd "edit" "same as unlock"

mkcmd :: String -> String -> Command
mkcmd n d = withGlobalOptions [jsonOptions, annexedMatchingOptions] $
	command n SectionCommon d paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = withFilesInGit (commandAction . whenAnnexed start) =<< workTreeItems ps

{- Before v6, the unlock subcommand replaces the symlink with a copy of
 - the file's content. In v6 and above, it converts the file from a symlink
 - to a pointer. -}
start :: RawFilePath -> Key -> CommandStart
start file key = ifM (isJust <$> isAnnexLink file)
	( starting "unlock" (mkActionItem (key, AssociatedFile (Just file))) $
		perform file key
	, stop
	)

perform :: RawFilePath -> Key -> CommandPerform
perform dest key = do
	destmode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus dest
	replaceFile (fromRawFilePath dest) $ \tmp ->
		ifM (inAnnex key)
			( do
				r <- linkFromAnnex key tmp destmode
				case r of
					LinkAnnexOk -> return ()
					LinkAnnexNoop -> return ()
					LinkAnnexFailed -> error "unlock failed"
			, liftIO $ writePointerFile (toRawFilePath tmp) key destmode
			)
	next $ cleanup dest key destmode

cleanup ::  RawFilePath -> Key -> Maybe FileMode -> CommandCleanup
cleanup dest key destmode = do
	stagePointerFile dest destmode =<< hashPointerFile key
	Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath (fromRawFilePath dest))
	return True

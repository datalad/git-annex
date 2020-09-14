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
seek ps = withFilesInGitAnnex ww seeker =<< workTreeItems ww ps
  where
	ww = WarnUnmatchLsFiles
	seeker = AnnexedFileSeeker
		{ startAction = start
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

start :: SeekInput -> RawFilePath -> Key -> CommandStart
start si file key = ifM (isJust <$> isAnnexLink file)
	( starting "unlock" ai si $ perform file key
	, stop
	)
  where
	ai = mkActionItem (key, AssociatedFile (Just file))

perform :: RawFilePath -> Key -> CommandPerform
perform dest key = do
	destmode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus dest
	replaceWorkTreeFile (fromRawFilePath dest) $ \tmp ->
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
	Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath dest)
	return True

{- git-annex command
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Unlock where

import Command
import Annex.Content
import Annex.Perms
import Annex.Link
import Annex.ReplaceFile
import Annex.InodeSentinal
import Utility.InodeCache
import Git.FilePath
import qualified Database.Keys
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (fileMode)

cmd :: Command
cmd = mkcmd "unlock" "unlock files for modification"

editcmd :: Command
editcmd = mkcmd "edit" "same as unlock"

mkcmd :: String -> String -> Command
mkcmd n d = withAnnexOptions [jsonOptions, annexedMatchingOptions] $
	command n SectionCommon d paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = withFilesInGitAnnex ww seeker =<< workTreeItems ww ps
  where
	ww = WarnUnmatchLsFiles "unlock"
	seeker = AnnexedFileSeeker
		{ startAction = const start
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

start :: SeekInput -> OsPath -> Key -> CommandStart
start si file key = ifM (isJust <$> isAnnexLink file)
	( starting "unlock" ai si $ perform file key
	, stop
	)
  where
	ai = mkActionItem (key, AssociatedFile (Just file))

perform :: OsPath -> Key -> CommandPerform
perform dest key = do
	destmode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus (fromOsPath dest)
	destic <- replaceWorkTreeFile dest $ \tmp -> do
		ifM (inAnnex key)
			( do
				r <- linkFromAnnex' key tmp destmode
				case r of
					LinkAnnexOk -> return ()
					LinkAnnexNoop -> return ()
					LinkAnnexFailed -> giveup "unlock failed"
			, liftIO $ writePointerFile tmp key destmode
			)
		withTSDelta (liftIO . genInodeCache tmp)
	next $ cleanup dest destic key destmode

cleanup :: OsPath -> Maybe InodeCache -> Key -> Maybe FileMode -> CommandCleanup
cleanup dest destic key destmode = do
	stagePointerFile dest destmode =<< hashPointerFile key
	maybe noop (restagePointerFile (Restage True) dest) destic
	Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath dest)
	return True

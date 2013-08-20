{- git-annex command
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Import where

import System.PosixCompat.Files

import Common.Annex
import Command
import qualified Annex
import qualified Command.Add
import qualified Option
import Utility.CopyFile

def :: [Command]
def = [withOptions opts $ notBareRepo $ command "import" paramPaths seek
	SectionCommon "move and add files from outside git working copy"]

opts :: [Option]
opts =
	[ duplicateOption
	, deduplicateOption
	, cleanDuplicatesOption
	]

duplicateOption :: Option
duplicateOption = Option.flag [] "duplicate" "do not delete outside files"

deduplicateOption :: Option
deduplicateOption = Option.flag [] "deduplicate" "do not add files whose content has been seen"

cleanDuplicatesOption :: Option
cleanDuplicatesOption = Option.flag [] "clean-duplicates" "delete outside duplicate files (import nothing)"

data DuplicateMode = Default | Duplicate | DeDuplicate | CleanDuplicates
	deriving (Eq)

getDuplicateMode :: Annex DuplicateMode
getDuplicateMode = gen
	<$> getflag duplicateOption
	<*> getflag deduplicateOption
	<*> getflag cleanDuplicatesOption
  where
  	getflag = Annex.getFlag . Option.name
  	gen False False False = Default
	gen True False False = Duplicate
	gen False True False = DeDuplicate
	gen False False True = CleanDuplicates
	gen _ _ _ = error "bad combination of --duplicate, --deduplicate, --clean-duplicates"

seek :: [CommandSeek]
seek = [withValue getDuplicateMode $ \mode -> withPathContents $ start mode]

start :: DuplicateMode -> (FilePath, FilePath) -> CommandStart
start mode (srcfile, destfile) =
	ifM (liftIO $ isRegularFile <$> getSymbolicLinkStatus srcfile)
		( do
			showStart "import" destfile
			next $ perform mode srcfile destfile
		, stop
		)

perform :: DuplicateMode -> FilePath -> FilePath -> CommandPerform
perform mode srcfile destfile = do
	whenM (liftIO $ doesFileExist destfile) $
		unlessM (Annex.getState Annex.force) $
			error $ "not overwriting existing " ++ destfile ++
				" (use --force to override)"

	liftIO $ createDirectoryIfMissing True (parentDir destfile)
	liftIO $ if mode == Duplicate
		then void $ copyFileExternal srcfile destfile
		else moveFile srcfile destfile
	Command.Add.perform destfile

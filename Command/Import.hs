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
import Backend
import Remote
import Types.KeySource

def :: [Command]
def = [withOptions opts $ notBareRepo $ command "import" paramPaths seek
	SectionCommon "move and add files from outside git working copy"]

opts :: [Option]
opts =
	[ duplicateOption
	, deduplicateOption
	, cleanDuplicatesOption
	, skipDuplicatesOption
	]

duplicateOption :: Option
duplicateOption = Option.flag [] "duplicate" "do not delete source files"

deduplicateOption :: Option
deduplicateOption = Option.flag [] "deduplicate" "delete source files whose content was imported before"

cleanDuplicatesOption :: Option
cleanDuplicatesOption = Option.flag [] "clean-duplicates" "delete duplicate source files (import nothing)"

skipDuplicatesOption :: Option
skipDuplicatesOption = Option.flag [] "skip-duplicates" "import only new files"

data DuplicateMode = Default | Duplicate | DeDuplicate | CleanDuplicates | SkipDuplicates
	deriving (Eq)

getDuplicateMode :: Annex DuplicateMode
getDuplicateMode = gen
	<$> getflag duplicateOption
	<*> getflag deduplicateOption
	<*> getflag cleanDuplicatesOption
	<*> getflag skipDuplicatesOption
  where
  	getflag = Annex.getFlag . Option.name
  	gen False False False False = Default
	gen True False False False = Duplicate
	gen False True False False = DeDuplicate
	gen False False True False = CleanDuplicates
	gen False False False True = SkipDuplicates
	gen _ _ _ _ = error "bad combination of --duplicate, --deduplicate, --clean-duplicates, --skip-duplicates"

seek :: [CommandSeek]
seek = [withValue getDuplicateMode $ \mode -> withPathContents $ start mode]

start :: DuplicateMode -> (FilePath, FilePath) -> CommandStart
start mode (srcfile, destfile) =
	ifM (liftIO $ isRegularFile <$> getSymbolicLinkStatus srcfile)
		( do
			isdup <- do
				backend <- chooseBackend destfile
				let ks = KeySource srcfile srcfile Nothing
				v <- genKey ks backend
				case v of
					Just (k, _) -> not . null <$> keyLocations k
					_ -> return False
			case pickaction isdup of
				Nothing -> stop
				Just a -> do
					showStart "import" destfile
					next a
		, stop
		)
  where
	deletedup = do
		showNote "duplicate"
		liftIO $ removeFile srcfile
		next $ return True
	importfile = do
		handleexisting =<< liftIO (catchMaybeIO $ getSymbolicLinkStatus destfile)
		liftIO $ createDirectoryIfMissing True (parentDir destfile)
		liftIO $ if mode == Duplicate || mode == SkipDuplicates
			then void $ copyFileExternal srcfile destfile
			else moveFile srcfile destfile
		Command.Add.perform destfile
	handleexisting Nothing = noop
	handleexisting (Just s)
		| isDirectory s = notoverwriting "(is a directory)"
		| otherwise = ifM (Annex.getState Annex.force) $
			( liftIO $ nukeFile destfile
			, notoverwriting "(use --force to override)"
			)
	notoverwriting why = error $ "not overwriting existing " ++ destfile ++ " " ++ why
	pickaction isdup = case mode of
		DeDuplicate
			| isdup -> Just deletedup
			| otherwise -> Just importfile
		CleanDuplicates
			| isdup -> Just deletedup
			| otherwise -> Nothing
		SkipDuplicates
			| isdup -> Nothing
			| otherwise -> Just importfile
		_ -> Just importfile


{- git-annex command
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Import where

import Common.Annex
import Command
import qualified Git
import qualified Annex
import qualified Command.Add
import Utility.CopyFile
import Backend
import Remote
import Types.KeySource
import Types.Key
import Annex.CheckIgnore
import Annex.NumCopies
import Types.TrustLevel
import Logs.Trust

cmd :: Command
cmd = withGlobalOptions fileMatchingOptions $ notBareRepo $
	command "import" SectionCommon 
		"move and add files from outside git working copy"
		paramPaths (seek <$$> optParser)

data DuplicateMode = Default | Duplicate | DeDuplicate | CleanDuplicates | SkipDuplicates
	deriving (Eq)

data ImportOptions = ImportOptions
	{ importFiles :: CmdParams
	, duplicateMode :: DuplicateMode
	}

optParser :: CmdParamsDesc -> Parser ImportOptions
optParser desc = ImportOptions
	<$> cmdParams desc
	<*> (fromMaybe Default <$> optional duplicateModeParser)

duplicateModeParser :: Parser DuplicateMode
duplicateModeParser = 
	flag' Duplicate
		( long "duplicate" 
		<> help "do not delete source files"
		)
	<|> flag' DeDuplicate
		( long "deduplicate"
		<> help "delete source files whose content was imported before"
		)
	<|> flag' CleanDuplicates
		( long "clean-duplicates"
		<> help "delete duplicate source files (import nothing)"
		)
	<|> flag' SkipDuplicates
		( long "skip-duplicates"
		<> help "import only new files"
		)

seek :: ImportOptions -> CommandSeek
seek o = do
	repopath <- liftIO . absPath =<< fromRepo Git.repoPath
	inrepops <- liftIO $ filter (dirContains repopath) <$> mapM absPath (importFiles o)
	unless (null inrepops) $ do
		error $ "cannot import files from inside the working tree (use git annex add instead): " ++ unwords inrepops
	withPathContents (start (duplicateMode o)) (importFiles o)

start :: DuplicateMode -> (FilePath, FilePath) -> CommandStart
start mode (srcfile, destfile) =
	ifM (liftIO $ isRegularFile <$> getSymbolicLinkStatus srcfile)
		( do
			ma <- pickaction
			case ma of
				Nothing -> stop
				Just a -> do
					showStart "import" destfile
					next a
		, stop
		)
  where
	deletedup k = do
		showNote $ "duplicate of " ++ key2file k
		verifyExisting k destfile
			( do
				liftIO $ removeFile srcfile
				next $ return True
			, do
				warning "Could not verify that the content is still present in the annex; not removing from the import location."
				stop
			)
	importfile = do
		ignored <- not <$> Annex.getState Annex.force <&&> checkIgnored destfile
		if ignored
			then do
				warning $ "not importing " ++ destfile ++ " which is .gitignored (use --force to override)"
				stop
			else do
				existing <- liftIO (catchMaybeIO $ getSymbolicLinkStatus destfile)
				case existing of
					Nothing -> importfilechecked
					(Just s)
						| isDirectory s -> notoverwriting "(is a directory)"
						| otherwise -> ifM (Annex.getState Annex.force)
							( do
								liftIO $ nukeFile destfile
								importfilechecked
							, notoverwriting "(use --force to override, or a duplication option such as --deduplicate to clean up)"
							)
	importfilechecked = do
		liftIO $ createDirectoryIfMissing True (parentDir destfile)
		liftIO $ if mode == Duplicate || mode == SkipDuplicates
			then void $ copyFileExternal CopyAllMetaData srcfile destfile
			else moveFile srcfile destfile
		Command.Add.perform destfile
	notoverwriting why = do
		warning $ "not overwriting existing " ++ destfile ++ " " ++ why
		stop
	checkdup dupa notdupa = do
		backend <- chooseBackend destfile
		let ks = KeySource srcfile srcfile Nothing
		v <- genKey ks backend
		case v of
			Just (k, _) -> ifM (not . null <$> keyLocations k)
				( return (maybe Nothing (\a -> Just (a k)) dupa)
				, return notdupa
				)
			_ -> return notdupa
	pickaction = case mode of
		DeDuplicate -> checkdup (Just deletedup) (Just importfile)
		CleanDuplicates -> checkdup (Just deletedup) Nothing
		SkipDuplicates -> checkdup Nothing (Just importfile)
		_ -> return (Just importfile)

verifyExisting :: Key -> FilePath -> (CommandPerform, CommandPerform) -> CommandPerform
verifyExisting key destfile (yes, no) = do
	-- Look up the numcopies setting for the file that it would be
	-- imported to, if it were imported.
	need <- getFileNumCopies destfile

	(tocheck, preverified) <- verifiableCopies key []
	verifyEnoughCopiesToDrop [] key need [] preverified tocheck
		(const yes) no

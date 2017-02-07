{- git-annex command
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Import where

import Command
import qualified Git
import qualified Annex
import qualified Command.Add
import Utility.CopyFile
import Backend
import Remote
import Types.KeySource
import Annex.CheckIgnore
import Annex.NumCopies
import Annex.FileMatcher
import Logs.Location

cmd :: Command
cmd = withGlobalOptions (jobsOption : jsonOption : fileMatchingOptions) $ notBareRepo $
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
seek o = allowConcurrentOutput $ do
	repopath <- liftIO . absPath =<< fromRepo Git.repoPath
	inrepops <- liftIO $ filter (dirContains repopath) <$> mapM absPath (importFiles o)
	unless (null inrepops) $ do
		giveup $ "cannot import files from inside the working tree (use git annex add instead): " ++ unwords inrepops
	largematcher <- largeFilesMatcher
	withPathContents (start largematcher (duplicateMode o)) (importFiles o)

start :: GetFileMatcher -> DuplicateMode -> (FilePath, FilePath) -> CommandStart
start largematcher mode (srcfile, destfile) =
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
	importfile = checkdestdir $ do
		ignored <- not <$> Annex.getState Annex.force <&&> checkIgnored destfile
		if ignored
			then do
				warning $ "not importing " ++ destfile ++ " which is .gitignored (use --force to override)"
				stop
			else do
				existing <- liftIO (catchMaybeIO $ getSymbolicLinkStatus destfile)
				case existing of
					Nothing -> importfilechecked
					Just s
						| isDirectory s -> notoverwriting "(is a directory)"
						| isSymbolicLink s -> notoverwriting "(is a symlink)"
						| otherwise -> ifM (Annex.getState Annex.force)
							( do
								liftIO $ nukeFile destfile
								importfilechecked
							, notoverwriting "(use --force to override, or a duplication option such as --deduplicate to clean up)"
							)
	checkdestdir cont = do
		let destdir = parentDir destfile
		existing <- liftIO (catchMaybeIO $ getSymbolicLinkStatus destdir)
		case existing of
			Nothing -> cont
			Just s
				| isDirectory s -> cont
				| otherwise -> do
					warning $ "not importing " ++ destfile ++ " because " ++ destdir ++ " is not a directory"
					stop

	importfilechecked = do
		liftIO $ createDirectoryIfMissing True (parentDir destfile)
		liftIO $ if mode == Duplicate || mode == SkipDuplicates
			then void $ copyFileExternal CopyAllMetaData srcfile destfile
			else moveFile srcfile destfile
		ifM (checkFileMatcher largematcher destfile)
			( Command.Add.perform destfile
			, next $ Command.Add.addSmall destfile 
			)
	notoverwriting why = do
		warning $ "not overwriting existing " ++ destfile ++ " " ++ why
		stop
	checkdup dupa notdupa = do
		backend <- chooseBackend destfile
		let ks = KeySource srcfile srcfile Nothing
		v <- genKey ks backend
		case v of
			Just (k, _) -> ifM (isKnownKey k)
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
	verifyEnoughCopiesToDrop [] key Nothing need [] preverified tocheck
		(const yes) no

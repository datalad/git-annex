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
cmd = withOptions opts $ notBareRepo $
	command "import" SectionCommon 
		"move and add files from outside git working copy"
		paramPaths (withParams seek)

opts :: [GlobalOption]
opts = duplicateModeOptions ++ fileMatchingOptions

data DuplicateMode = Default | Duplicate | DeDuplicate | CleanDuplicates | SkipDuplicates
	deriving (Eq, Enum, Bounded)

associatedOption :: DuplicateMode -> Maybe Option
associatedOption Default = Nothing
associatedOption Duplicate = Just $
	flagOption [] "duplicate" "do not delete source files"
associatedOption DeDuplicate = Just $
	flagOption [] "deduplicate" "delete source files whose content was imported before"
associatedOption CleanDuplicates = Just $
	flagOption [] "clean-duplicates" "delete duplicate source files (import nothing)"
associatedOption SkipDuplicates = Just $
	flagOption [] "skip-duplicates" "import only new files"

duplicateModeOptions :: [Option]
duplicateModeOptions = mapMaybe associatedOption [minBound..maxBound]

getDuplicateMode :: Annex DuplicateMode
getDuplicateMode = go . catMaybes <$> mapM getflag [minBound..maxBound]
  where
	getflag m = case associatedOption m of
		Nothing -> return Nothing
		Just o -> ifM (Annex.getFlag (optionName o))
			( return (Just m)
			, return Nothing
			)
	go [] = Default
	go [m] = m
	go ms = error $ "cannot combine " ++
		unwords (map (optionParam . fromJust . associatedOption) ms)

seek :: CmdParams -> CommandSeek
seek ps = do
	mode <- getDuplicateMode
	repopath <- liftIO . absPath =<< fromRepo Git.repoPath
	inrepops <- liftIO $ filter (dirContains repopath) <$> mapM absPath ps
	unless (null inrepops) $ do
		error $ "cannot import files from inside the working tree (use git annex add instead): " ++ unwords inrepops
	withPathContents (start mode) ps

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
		ifM (verifiedExisting k destfile)
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

verifiedExisting :: Key -> FilePath -> Annex Bool
verifiedExisting key destfile = do
	-- Look up the numcopies setting for the file that it would be
	-- imported to, if it were imported.
	need <- getFileNumCopies destfile

	(remotes, trusteduuids) <- knownCopies key
	untrusteduuids <- trustGet UnTrusted
	let tocheck = Remote.remotesWithoutUUID remotes (trusteduuids++untrusteduuids)
	verifyEnoughCopies [] key need [] trusteduuids tocheck

{- git-annex command
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.AddComputed where

import Command
import qualified Git
import qualified Annex
import qualified Remote.Compute
import qualified Types.Remote as Remote
import Backend
import Annex.CatFile
import Annex.Content.Presence
import Annex.Ingest
import Types.KeySource
import Messages.Progress
import Logs.Location
import Utility.Metered
import Backend.URL (fromUrl)

import qualified Data.Map as M
import Data.Time.Clock

cmd :: Command
cmd = notBareRepo $ 
	command "addcomputed" SectionCommon "add computed files to annex"
		(paramRepeating paramExpression)
		(seek <$$> optParser)

data AddComputedOptions = AddComputedOptions
	{ computeParams :: CmdParams
	, computeRemote :: DeferredParse Remote
	, reproducible :: Maybe Reproducible
	}

optParser :: CmdParamsDesc -> Parser AddComputedOptions
optParser desc = AddComputedOptions
	<$> cmdParams desc
	<*> (mkParseRemoteOption <$> parseToOption)
	<*> parseReproducible

newtype Reproducible = Reproducible { isReproducible :: Bool }

parseReproducible :: Parser (Maybe Reproducible)
parseReproducible = r <|> unr
  where
	r  = flag Nothing (Just (Reproducible True))
		( long "reproducible"
		<> short 'r'
		<> help "computation is fully reproducible"
		)
	unr = flag Nothing (Just (Reproducible False))
		( long "unreproducible"
		<> short 'u'
		<> help "computation is not fully reproducible"
		)

seek :: AddComputedOptions -> CommandSeek
seek o = startConcurrency commandStages (seek' o)

seek' :: AddComputedOptions -> CommandSeek
seek' o = do
	r <- getParsed (computeRemote o)
	unless (Remote.Compute.isComputeRemote r) $
		giveup "That is not a compute remote."

	commandAction $ start o r

start :: AddComputedOptions -> Remote -> CommandStart
start o r = starting "addcomputed" ai si $ perform o r
  where
	ai = ActionItemUUID (Remote.uuid r) (UnquotedString (Remote.name r))
	si = SeekInput (computeParams o)

perform :: AddComputedOptions -> Remote -> CommandPerform
perform o r = do
	program <- Remote.Compute.getComputeProgram r
	repopath <- fromRepo Git.repoPath
	subdir <- liftIO $ relPathDirToFile repopath (literalOsPath ".")
	let state = Remote.Compute.ComputeState
		{ Remote.Compute.computeParams = computeParams o ++
			Remote.Compute.defaultComputeParams r
		, Remote.Compute.computeInputs = mempty
		, Remote.Compute.computeOutputs = mempty
		, Remote.Compute.computeSubdir = subdir
		, Remote.Compute.computeReproducible = False
		}
	fast <- Annex.getRead Annex.fast
	showOutput
	Remote.Compute.runComputeProgram program state
		(Remote.Compute.ImmutableState False)
		(getInputContent fast)
		(addComputed "adding" True r (reproducible o) (const True) fast)
	next $ return True

addComputed :: StringContainingQuotedPath -> Bool -> Remote -> Maybe Reproducible -> (OsPath -> Bool) -> Bool -> Remote.Compute.ComputeState -> OsPath -> NominalDiffTime -> Annex ()
addComputed addaction stagefiles r reproducibleconfig wantfile fast state tmpdir ts = do
	let outputs = Remote.Compute.computeOutputs state
	when (M.null outputs) $
		giveup "The computation succeeded, but it did not generate any files."
	oks <- forM (M.keys outputs) $ \outputfile -> do
		showAction $ addaction <> " " <> QuotedPath outputfile
		k <- catchNonAsync (addfile outputfile)
			(\err -> giveup $ "Failed to ingest output file " ++ fromOsPath outputfile ++ ": " ++ show err)
		return (outputfile, Just k)
	let state' = state
		{ Remote.Compute.computeOutputs = M.fromList oks
		}
	forM_ (mapMaybe snd oks) $ \k -> do
		Remote.Compute.setComputeState
			(Remote.remoteStateHandle r)
			k ts state'
		logChange NoLiveUpdate k (Remote.uuid r) InfoPresent
  where
	addfile outputfile
		| fast = do
			when (wantfile outputfile) $
				if stagefiles
					then addSymlink outputfile stateurlk Nothing
					else makelink stateurlk
			return stateurlk
		| isreproducible = do
			sz <- liftIO $ getFileSize outputfile'
			metered Nothing sz Nothing $ \_ p ->
				if wantfile outputfile
					then ingesthelper p Nothing
					else genkey p
		| otherwise =
			if wantfile outputfile
				then ingesthelper nullMeterUpdate
					(Just stateurlk)
				else return stateurlk
	  where
	  	stateurl = Remote.Compute.computeStateUrl r state outputfile
		stateurlk = fromUrl stateurl Nothing True
		outputfile' = tmpdir </> outputfile
		ld = LockedDown ldc ks
		ks = KeySource
			{ keyFilename = outputfile
			, contentLocation = outputfile'
			, inodeCache = Nothing
			}
		ingestwith a = a >>= \case
			Nothing -> giveup "ingestion failed"
			Just k -> do
				logStatus NoLiveUpdate k InfoPresent
				return k
		genkey p = do
			backend <- chooseBackend outputfile
			fst <$> genKey ks p backend
		makelink k = void $ makeLink outputfile k Nothing
		ingesthelper p mk
			| stagefiles = ingestwith $
				ingestAdd' p (Just ld) mk
			| otherwise = ingestwith $ do
				mk' <- fst <$> ingest p (Just ld) mk
				maybe noop makelink mk'
				return mk'
	
	ldc = LockDownConfig
		{ lockingFile = True
		, hardlinkFileTmpDir = Nothing
		, checkWritePerms = True
		}
	
	isreproducible = case reproducibleconfig of
		Just v -> isReproducible v
		Nothing -> Remote.Compute.computeReproducible state
	
getInputContent :: Bool -> OsPath -> Annex (Key, Maybe OsPath)
getInputContent fast p = catKeyFile p >>= \case
	Just inputkey -> getInputContent' fast inputkey (fromOsPath p)
	Nothing -> ifM (liftIO $ doesFileExist p)
		( giveup $ "The computation needs an input file that is not an annexed file: " ++ fromOsPath p 
		, giveup $ "The computation needs an input file which does not exist: " ++ fromOsPath p
		)

getInputContent' :: Bool -> Key -> String -> Annex (Key, Maybe OsPath)
getInputContent' fast inputkey filedesc = do
	obj <- calcRepo (gitAnnexLocation inputkey)
	if fast
		then return (inputkey, Nothing)
		else ifM (inAnnex inputkey)
			( return (inputkey, Just obj)
			, giveup $ "The computation needs the content of a file which is not present: " ++ filedesc
			)

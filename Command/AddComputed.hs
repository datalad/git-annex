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
import qualified Git.Types as Git
import qualified Git.Ref as Git
import qualified Annex
import qualified Remote.Compute
import qualified Types.Remote as Remote
import Backend
import Annex.CatFile
import Annex.Content.Presence
import Annex.Ingest
import Annex.UUID
import Annex.GitShaKey
import Types.KeySource
import Types.Key
import Messages.Progress
import Logs.Location
import Logs.EquivilantKeys
import Utility.Metered
import Backend.URL (fromUrl)

import qualified Data.Map as M
import Data.Time.Clock

cmd :: Command
cmd = notBareRepo $ withAnnexOptions [backendOption] $
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
	deriving (Show, Eq)

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
		}
	fast <- Annex.getRead Annex.fast
	Remote.Compute.runComputeProgram program state
		(Remote.Compute.ImmutableState False)
		(getInputContent fast)
		Nothing
		(addComputed (Just "adding") True r (reproducible o) chooseBackend Just fast)
	next $ return True

addComputed
	:: Maybe StringContainingQuotedPath
	-> Bool
	-> Remote
	-> Maybe Reproducible
	-> (OsPath -> Annex Backend)
	-> (OsPath -> Maybe OsPath)
	-> Bool
	-> Remote.Compute.ComputeProgramResult
	-> OsPath
	-> NominalDiffTime
	-> Annex ()
addComputed maddaction stagefiles r reproducibleconfig choosebackend destfile fast result tmpdir ts = do
	when (M.null outputs) $
		giveup "The computation succeeded, but it did not generate any files."
	oks <- forM (M.keys outputs) $ \outputfile -> do
		case maddaction of
			Just addaction -> showAction $
				addaction <> " " <> QuotedPath outputfile
			Nothing -> noop
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

		let u = Remote.uuid r
		unlessM (elem u <$> loggedLocations k) $
			logChange NoLiveUpdate k u InfoPresent
  where
	state = Remote.Compute.computeState result
	
	outputs = Remote.Compute.computeOutputs state
	
	addfile outputfile
		| fast = do
			case destfile outputfile of
				Nothing -> noop
				Just f
					| stagefiles -> addSymlink f stateurlk Nothing
					| otherwise -> makelink f stateurlk
			return stateurlk
		| isreproducible = do
			sz <- liftIO $ getFileSize outputfile'
			metered Nothing sz Nothing $ \_ p ->
				case destfile outputfile of
					Just f -> ingesthelper f p Nothing
					Nothing -> genkey outputfile p
		| otherwise = case destfile outputfile of
			Just f -> ingesthelper f nullMeterUpdate
				(Just stateurlk)
			Nothing -> return stateurlk
	  where
	  	stateurl = Remote.Compute.computeStateUrl r state outputfile
		stateurlk = fromUrl stateurl Nothing True
		outputfile' = tmpdir </> outputfile
		ld f = LockedDown ldc (ks f)
		ks f = KeySource
			{ keyFilename = f
			, contentLocation = outputfile'
			, inodeCache = Nothing
			}
		genkey f p = do
			backend <- choosebackend outputfile
			fst <$> genKey (ks f) p backend
		makelink f k = void $ makeLink f k Nothing
		ingesthelper f p mk
			| stagefiles = ingestwith $ do
				k <- maybe (genkey f p) return mk
				ingestAdd' p (Just (ld f)) (Just k)
			| otherwise = ingestwith $ do
				k <- maybe (genkey f p) return mk
				mk' <- fst <$> ingest p (Just (ld f)) (Just k)
				maybe noop (makelink f) mk'
				return mk'
		ingestwith a = a >>= \case
			Nothing -> giveup "ingestion failed"
			Just k -> do
				u <- getUUID
				unlessM (elem u <$> loggedLocations k) $
					logStatus NoLiveUpdate k InfoPresent
				when (fromKey keyVariety k == VURLKey) $ do
					hb <- hashBackend
					void $ addEquivilantKey hb k 
						=<< calcRepo (gitAnnexLocation k)
				return k
	
	ldc = LockDownConfig
		{ lockingFile = True
		, hardlinkFileTmpDir = Nothing
		, checkWritePerms = True
		}
	
	isreproducible = case reproducibleconfig of
		Just v -> isReproducible v
		Nothing -> Remote.Compute.computeReproducible result
	
getInputContent :: Bool -> OsPath -> Bool -> Annex (Key, Maybe (Either Git.Sha OsPath))
getInputContent fast p required = catKeyFile p >>= \case
	Just inputkey -> getInputContent' fast inputkey required filedesc
	Nothing -> inRepo (Git.fileRef p) >>= \case
		Just fileref -> catObjectMetaData fileref >>= \case
			Just (sha, _, t)
				| t == Git.BlobObject ->
					getInputContent' fast (gitShaKey sha) required filedesc
				| otherwise ->
					badinput $ ", not a git " ++ decodeBS (Git.fmtObjectType t)
			Nothing -> notcheckedin
		Nothing -> notcheckedin
  where
	filedesc = fromOsPath p
	badinput s = giveup $ "The computation needs an input file " ++ s ++ ": " ++ fromOsPath p
	notcheckedin = badinput "that is not checked into the git repository"

getInputContent' :: Bool -> Key -> Bool -> String -> Annex (Key, Maybe (Either Git.Sha OsPath))
getInputContent' fast inputkey required filedesc
	| fast && not required = return (inputkey, Nothing)
	| otherwise = case keyGitSha inputkey of
		Nothing -> ifM (inAnnex inputkey)
			( do
				obj <- calcRepo (gitAnnexLocation inputkey)
				return (inputkey, Just (Right obj))
			, giveup $ "The computation needs the content of an annexed file which is not present: " ++ filedesc
			)
		Just sha -> return (inputkey, Just (Left sha))

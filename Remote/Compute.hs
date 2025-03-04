{- Compute remote.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Compute (
	remote,
	isComputeRemote,
	ComputeState(..),
	setComputeState,
	getComputeState,
	computeStateUrl,
	ComputeProgram,
	getComputeProgram,
	runComputeProgram,
	ImmutableState(..),
	computationBehaviorChangeError,
	defaultComputeParams,
) where

import Annex.Common
import qualified Annex
import Types.Remote
import Types.ProposedAccepted
import Types.MetaData
import Types.Creds
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.List.Util
import Annex.SpecialRemote.Config
import Annex.UUID
import Annex.Content
import Annex.Tmp
import Annex.GitShaKey
import Annex.CatFile
import Annex.RepoSize.LiveUpdate
import qualified Annex.Transfer
import Logs.MetaData
import Logs.EquivilantKeys
import Logs.Location
import Utility.Metered
import Utility.TimeStamp
import Utility.Env
import Utility.Tmp.Dir
import Utility.Url
import Utility.MonotonicClock
import Types.Key
import Backend
import qualified Git
import qualified Utility.FileIO as F
import qualified Utility.SimpleProtocol as Proto

import Network.HTTP.Types.URI
import Data.Time.Clock
import Text.Read
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

remote :: RemoteType
remote = RemoteType
	{ typename = "compute"
	, enumerate = const $ findSpecialRemotes "compute"
	, generate = gen
	, configParser = computeConfigParser
	, setup = setupInstance
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

isComputeRemote :: Remote -> Bool
isComputeRemote r = typename (remotetype r) == typename remote

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = case getComputeProgram' rc of
	Left _err -> return Nothing
	Right program -> do
		c <- parsedRemoteConfig remote rc
		cst <- remoteCost gc c veryExpensiveRemoteCost
		return $ Just $ mk program c cst
  where
	mk program c cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyUnsupported
 		, retrieveKeyFile = computeKey rs program
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey rs
		, lockContent = Nothing
		, checkPresent = checkKey rs
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, getRepo = return r
		, readonly = True
		, appendonly = False
		, untrustworthy = False
		, availability = pure LocallyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

setupInstance :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
setupInstance ss mu _ c _ = do
	ComputeProgram program <- either giveup return $ getComputeProgram' c
	allowedprograms <- maybe [] words . annexAllowedComputePrograms
		<$> Annex.getGitConfig
	case ss of
		Init -> noop
		_ -> unless (program `elem` allowedprograms) $ do
			let remotename = fromMaybe "(unknown)" (lookupName c)
			giveup $ unwords
				[ "Unable to enable compute special remote"
				, remotename
				, "because its compute program"
				, program
				, "is not listed in annex.security-allowed-compute-programs"
				]
	unlessM (liftIO $ inSearchPath program) $
		giveup $ "Cannot find " ++ program ++ " in PATH"
	u <- maybe (liftIO genUUID) return mu
	gitConfigSpecialRemote u c [("compute", "true")]
	return (c, u)

computeConfigParser :: RemoteConfig -> Annex RemoteConfigParser
computeConfigParser _ = return $ RemoteConfigParser
	{ remoteConfigFieldParsers = 
		[ optionalStringParser programField
			(FieldDesc $ "compute program (must start with \"" ++ safetyPrefix ++ "\")")
		]
	-- Pass through all other params, which git-annex addcomputed adds
	-- to the input params.
	, remoteConfigRestPassthrough = Just
		( const True
		, [("*", FieldDesc "all other parameters are passed to compute program")]
		)
	}

defaultComputeParams :: Remote -> [String]
defaultComputeParams = map mk . M.toList . getRemoteConfigPassedThrough . config
  where
	mk (f, v) = fromProposedAccepted f ++ '=' : v

newtype ComputeProgram = ComputeProgram String
	deriving (Show)

getComputeProgram :: Remote -> Annex ComputeProgram
getComputeProgram r = 
	case getComputeProgram' (unparsedRemoteConfig (config r)) of
		Right program -> return program
		Left err -> giveup $ 
			"Problem with the configuration of compute remote " ++ name r ++ ": " ++ err

getComputeProgram' :: RemoteConfig -> Either String ComputeProgram
getComputeProgram' c = case fromProposedAccepted <$> M.lookup programField c of
	Just program
		| safetyPrefix `isPrefixOf` program ->
			Right (ComputeProgram program)
		| otherwise -> Left $
			"The program's name must begin with \"" ++ safetyPrefix ++ "\""
	Nothing -> Left "Specify program="

-- Limiting the program to "git-annex-compute-" prefix is important for
-- security, it prevents autoenabled compute remotes from running arbitrary
-- programs.
safetyPrefix :: String
safetyPrefix = "git-annex-compute-"

programField :: RemoteConfigField
programField = Accepted "program"

data ProcessCommand
	= ProcessInput FilePath
	| ProcessOutput FilePath
	| ProcessReproducible
	| ProcessProgress PercentFloat
	deriving (Show, Eq)

instance Proto.Receivable ProcessCommand where
	parseCommand "INPUT" = Proto.parse1 ProcessInput
	parseCommand "OUTPUT" = Proto.parse1 ProcessOutput
	parseCommand "REPRODUCIBLE" = Proto.parse0 ProcessReproducible
	parseCommand "PROGRESS" = Proto.parse1 ProcessProgress
	parseCommand _ = Proto.parseFail

newtype PercentFloat = PercentFloat Float
	deriving (Show, Eq)

instance Proto.Serializable PercentFloat where
	serialize (PercentFloat p) = show p
	deserialize s = PercentFloat <$> readMaybe s

data ComputeState = ComputeState
	{ computeParams :: [String]
	, computeInputs :: M.Map OsPath Key
	, computeOutputs :: M.Map OsPath (Maybe Key)
	, computeSubdir :: OsPath
	, computeReproducible :: Bool
	, computeInputsUnavailable :: Bool
	}
	deriving (Show, Eq)

{- Formats a ComputeState as an URL query string.
 -
 - Prefixes computeParams with 'p', computeInputs with 'i',
 - and computeOutputs with 'o'. Uses "d" for computeSubdir.
 -
 - When the passed Key is an output, rather than duplicate it
 - in the query string, that output has no value.
 -
 - Example: "psomefile&pdestfile&pbaz&isomefile=WORM--foo&odestfile=&d=subdir"
 -
 - The computeParams are in the order they were given. The computeInputs
 - and computeOutputs are sorted in ascending order for stability.
 -}
formatComputeState :: Key -> ComputeState -> B.ByteString
formatComputeState k = formatComputeState' (Just k)

formatComputeState' :: Maybe Key -> ComputeState -> B.ByteString
formatComputeState' mk st = renderQuery False $ concat
	[ map formatparam (computeParams st)
	, map formatinput (M.toAscList (computeInputs st))
	, mapMaybe formatoutput (M.toAscList (computeOutputs st))
	, [("d", Just (fromOsPath (computeSubdir st)))]
	]
  where
	formatparam p = ("p" <> encodeBS p, Nothing)
	formatinput (file, key) =
		("i" <> fromOsPath file, Just (serializeKey' key))
	formatoutput (file, (Just key)) = Just $
		("o" <> fromOsPath file,
			if Just key == mk
				then Nothing
				else Just (serializeKey' key)
		)
	formatoutput (_, Nothing) = Nothing

parseComputeState :: Key -> B.ByteString -> Maybe ComputeState
parseComputeState k b =
	let st = go emptycomputestate (parseQuery b)
	in if st == emptycomputestate then Nothing else Just st
  where
	emptycomputestate = ComputeState mempty mempty mempty "." False False
	go :: ComputeState -> [QueryItem] -> ComputeState
	go c [] = c { computeParams = reverse (computeParams c) }
	go c ((f, v):rest) = 
		let c' = fromMaybe c $ case decodeBS f of
			('p':p) -> Just $ c
				{ computeParams = p : computeParams c
				}
			('i':i) -> do
				key <- deserializeKey' =<< v
				Just $ c
					{ computeInputs = 
						M.insert (toOsPath i) key
							(computeInputs c)
					}
			('o':o) -> case v of
				Just kv -> do
					key <- deserializeKey' kv
					Just $ c
						{ computeOutputs =
							M.insert (toOsPath o)
								(Just key)
								(computeOutputs c)
						}
				Nothing -> Just $ c
					{ computeOutputs = 
						M.insert (toOsPath o)
							(Just k)
							(computeOutputs c)
					}
			('d':[]) -> do
				subdir <- v
				Just $ c
					{ computeSubdir = toOsPath subdir
					}
			_ -> Nothing
		in go c' rest

{- A compute: url for a given output file of a computation. -}
computeStateUrl :: Remote -> ComputeState -> OsPath -> URLString
computeStateUrl r st p = 
	"annex-compute:" ++ fromUUID (uuid r) ++ "/" ++ fromOsPath p ++ "?" 
		++ decodeBS (formatComputeState' Nothing st')
  where
	-- Omit computeOutputs, so this gives the same result whether
	-- it's called on a ComputeState with the computeOutputs 
	-- Keys populated or not.
	st' = st { computeOutputs = mempty }

{- The per remote metadata is used to store ComputeState. This allows
 - recording multiple ComputeStates that generate the same key.
 -
 - The metadata fields are numbers (prefixed with "t" to make them legal
 - field names), which are estimates of how long it might take to run
 - the computation (in seconds).
 -}
setComputeState :: RemoteStateHandle -> Key -> NominalDiffTime -> ComputeState -> Annex ()
setComputeState rs k ts st = addRemoteMetaData k rs $ MetaData $ M.singleton
	(mkMetaFieldUnchecked $ T.pack ('t':show (truncateResolution 1 ts)))
	(S.singleton (MetaValue (CurrentlySet True) (formatComputeState k st)))

{- When multiple ComputeStates have been recorded for the same key,
 - this returns one that is probably less expensive to compute,
 - based on the original time it took to compute it. -}
getComputeState:: RemoteStateHandle -> Key -> Annex (Maybe ComputeState)
getComputeState rs k = headMaybe . map snd . sortOn fst
	<$> getComputeStatesUnsorted rs k

getComputeStatesUnsorted :: RemoteStateHandle -> Key -> Annex [(NominalDiffTime, ComputeState)]
getComputeStatesUnsorted rs k = do
	RemoteMetaData _ (MetaData m) <- getCurrentRemoteMetaData rs k
	return $ go [] (M.toList m)
  where
	go c [] = concat c
	go c ((f, s) : rest) =
		let sts = mapMaybe (parseComputeState k . fromMetaValue)
			(S.toList s)
		in case parsePOSIXTime (T.encodeUtf8 (T.drop 1 (fromMetaField f))) of
			Just ts -> go (zip (repeat ts) sts : c) rest
			Nothing -> go c rest

computeProgramEnvironment :: ComputeState -> Annex [(String, String)]
computeProgramEnvironment st = do
	environ <- filter (caninherit . fst) <$> liftIO getEnvironment
	let addenv = mapMaybe go (computeParams st)
	return $ environ ++ addenv
  where
	envprefix = "ANNEX_COMPUTE_"
	caninherit v = not (envprefix `isPrefixOf` v)
	go p
		| '=' `elem` p =
			let (f, v) = separate (== '=') p
			in Just (envprefix ++ f, v)
		| otherwise = Nothing

newtype ImmutableState = ImmutableState Bool

runComputeProgram
	:: ComputeProgram
	-> ComputeState
	-> ImmutableState
	-> (OsPath -> Annex (Key, Maybe (Either Git.Sha OsPath)))
	-- ^ get input file's content, or Nothing the input file's
	-- content is not available
	-> (ComputeState -> OsPath -> NominalDiffTime -> Annex v)
	-> Annex v
runComputeProgram (ComputeProgram program) state (ImmutableState immutablestate) getinputcontent cont =
	withOtherTmp $ \othertmpdir ->
		withTmpDirIn othertmpdir "compute" go
  where
	go tmpdir = do
		environ <- computeProgramEnvironment state
		subdir <- liftIO $ getsubdir tmpdir
		let pr = (proc program (computeParams state))
			 { cwd = Just (fromOsPath subdir)
			 , std_in = CreatePipe
			 , std_out = CreatePipe
			 , env = Just environ
			 }
		showOutput
		starttime <- liftIO currentMonotonicTimestamp
		state' <- bracket
			(liftIO $ createProcess pr)
			(liftIO . cleanupProcess)
			(getinput state tmpdir subdir)
		endtime <- liftIO currentMonotonicTimestamp
		cont state' subdir (calcduration starttime endtime)
		
	getsubdir tmpdir = do
		let subdir = tmpdir </> computeSubdir state
		ifM (dirContains <$> absPath tmpdir <*> absPath subdir)
			( do
				createDirectoryIfMissing True subdir
				return subdir
			-- Ignore unsafe value in state.
			, return tmpdir
			)
	
	getinput state' tmpdir subdir p = 
		liftIO (hGetLineUntilExitOrEOF (processHandle p) (stdoutHandle p)) >>= \case
			Just l
				| null l -> getinput state' tmpdir subdir p
				| otherwise -> do
					state'' <- parseoutput p tmpdir subdir state' l
					getinput state'' tmpdir subdir p
			Nothing -> do
				liftIO $ hClose (stdoutHandle p)
				liftIO $ hClose (stdinHandle p)
				unlessM (liftIO $ checkSuccessProcess (processHandle p)) $
					giveup $ program ++ " exited unsuccessfully"
				return state'
	
	parseoutput p tmpdir subdir state' l = case Proto.parseMessage l of
		Just (ProcessInput f) -> do
			let f' = toOsPath f
			let knowninput = M.member f' (computeInputs state')
			checksafefile tmpdir subdir f' "input"
			checkimmutable knowninput "inputting" f' $ do
				(k, inputcontent) <- getinputcontent f'
				mp <- case inputcontent of
					Nothing -> pure Nothing
					Just (Right f'') -> liftIO $
						Just <$> relPathDirToFile subdir f''
					Just (Left gitsha) ->
						Just <$> (liftIO . relPathDirToFile subdir 
							=<< populategitsha gitsha tmpdir)
				liftIO $ hPutStrLn (stdinHandle p) $
					maybe "" fromOsPath mp
				liftIO $ hFlush (stdinHandle p)
				let state'' = state'
					{ computeInputsUnavailable = 
						isNothing mp || computeInputsUnavailable state'
					}
				return $ if immutablestate
					then state''
					else state''
						{ computeInputs = 
							M.insert f' k
								(computeInputs state')
						}
		Just (ProcessOutput f) -> do
			let f' = toOsPath f
			checksafefile tmpdir subdir f' "output"
			let knownoutput = M.member f' (computeOutputs state')
			checkimmutable knownoutput "outputting" f' $ 
				return $ if immutablestate
					then state'
					else state'
						{ computeOutputs = 
							M.insert f' Nothing
								(computeOutputs state')
						}
		Just (ProcessProgress percent) -> do
			-- XXX
			return state'
		Just ProcessReproducible ->
			return $ state' { computeReproducible = True }
		Nothing -> giveup $
			program ++ " output included an unparseable line: \"" ++ l ++ "\""

	checksafefile tmpdir subdir f fileaction = do
		let err problem = giveup $
			program ++ " tried to " ++ fileaction ++ " a file that is " ++ problem ++ ": " ++ fromOsPath f
		unlessM (liftIO $ dirContains <$> absPath tmpdir <*> absPath (subdir </> f)) $
			err "outside the git repository"
		when (any (\p -> dropTrailingPathSeparator p == literalOsPath ".git") (splitPath f)) $
			err "inside the .git directory"

	checkimmutable True _ _ a = a
	checkimmutable False requestdesc p a
		| not immutablestate = a
		| otherwise = computationBehaviorChangeError (ComputeProgram program) requestdesc p
	
	calcduration (MonotonicTimestamp starttime) (MonotonicTimestamp endtime) =
		fromIntegral (endtime - starttime) :: NominalDiffTime

	-- Writes to a .git/objects/ file in the tmpdir, rather than
	-- using the input filename, to avoid exposing the input filename
	-- to the program as a parameter, which could parse it as a dashed
	-- option or other special parameter.
	populategitsha gitsha tmpdir = do
		let f = tmpdir </> ".git" </> "objects"
			</> toOsPath (Git.fromRef' gitsha)
		liftIO $ createDirectoryIfMissing True $ takeDirectory f
		liftIO . F.writeFile f =<< catObject gitsha
		return f

computationBehaviorChangeError :: ComputeProgram -> String -> OsPath -> Annex a
computationBehaviorChangeError (ComputeProgram program) requestdesc p =
	giveup $ program ++ " is not behaving the same way it used to, now " ++ requestdesc ++ ": " ++ fromOsPath p

computeKey :: RemoteStateHandle -> ComputeProgram -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
computeKey rs (ComputeProgram program) k _af dest p vc =
	getComputeState rs k >>= \case
		Just state -> 
			case computeskey state of
				Just keyfile -> runComputeProgram
					(ComputeProgram program)
					state
					(ImmutableState True)
					(getinputcontent state)
					(postcompute keyfile)
				Nothing -> missingstate
		Nothing -> missingstate
  where
	missingstate = giveup "Missing compute state"

	getinputcontent state f =
		case M.lookup (fromOsPath f) (computeInputs state) of
			Just inputkey -> case keyGitSha inputkey of
				Nothing -> 
					let retkey = do
						obj <- calcRepo (gitAnnexLocation inputkey)
						return (inputkey, Just (Right obj))
					in ifM (inAnnex inputkey)
						( retkey
						, ifM (getinputcontent' f inputkey)
							( retkey
							, return (inputkey, Nothing)
							)
						)
				Just gitsha ->
					return (inputkey, Just (Left gitsha))
			Nothing -> error "internal"
	
	getinputcontent' f inputkey = do
		remotes <- avoidCycles [k] inputkey
			=<< keyPossibilities inputkey
		anyM (getinputcontentfrom f inputkey) remotes
	
	getinputcontentfrom f inputkey r = do
		showAction $ "getting input " <> QuotedPath f
			<> " from " <> UnquotedString (name r)
		lu <- prepareLiveUpdate Nothing inputkey AddingKey
		logStatusAfter lu inputkey $
			Annex.Transfer.download r inputkey (AssociatedFile (Just f))
				Annex.Transfer.stdRetry Annex.Transfer.noNotification

	computeskey state = 
		case M.keys $ M.filter (== Just k) (computeOutputs state) of
			(keyfile : _) -> Just keyfile
			[] -> Nothing

	postcompute keyfile state tmpdir _ts
		| computeInputsUnavailable state = 
			giveup "Input file(s) unavailable."
		| otherwise = postcompute' keyfile state tmpdir

	postcompute' keyfile state tmpdir = do
		hb <- hashBackend
		let updatevurl key getobj = 
			if (fromKey keyVariety key == VURLKey)
				then addEquivilantKey hb key =<< getobj
				else return Nothing

		let keyfile' = tmpdir </> keyfile
		unlessM (liftIO $ doesFileExist keyfile') $
			giveup $ program ++ " exited sucessfully, but failed to write the computed file"
		catchNonAsync (liftIO $ moveFile keyfile' dest)
			(\err -> giveup $ "failed to move the computed file: " ++ show err)
		mverification <- updatevurl k (pure dest)

		-- Try to move any other computed object files into the annex.
		forM_ (M.toList $ computeOutputs state) $ \case
			(file, (Just key)) ->
				when (k /= key) $ do
					let file' = tmpdir </> file
					whenM (liftIO $ doesFileExist file') $ do
						whenM (verifyKeyContentPostRetrieval RetrievalAllKeysSecure vc MustVerify key file') $ do
							moved <- moveAnnex key file' `catchNonAsync` const (pure False)
							when moved $
								void $ updatevurl key (calcRepo (gitAnnexLocation key))
			_ -> noop

		-- The program might not be reproducible,
		-- so require strong verification.
		return $ fromMaybe MustVerify mverification
		
keyPossibilities :: Key -> Annex [Remote]
keyPossibilities key = do
	remotelist <- Annex.getState Annex.remotes
	locs <- loggedLocations key
	keyPossibilities' (IncludeIgnored False) key locs remotelist

{- Filter out any remotes that, in order to compute the inputkey, would
 - need to get the outputkey from some remote.
 -
 - This only finds cycles of compute special remotes, not any other
 - similar type of special remote that might have its own input keys.
 - There are no other such special remotes in git-annex itself, so this
 - is the best that can be done.
 -
 - Note that, in a case where a compute special remote needs the outputkey
 - to compute the inputkey, but could get the outputkey from either this
 - remote, or some other, non-compute remote, that is filtered out as a
 - cycle because it's not possible to prevent that remote getting from this
 - remote.
 -}
avoidCycles :: [Key] -> Key -> [Remote] -> Annex [Remote]
avoidCycles outputkeys inputkey = filterM go
  where
	go r
		| iscomputeremote r = 
			getComputeState (remoteStateHandle r) inputkey >>= \case
				Nothing -> return True
				Just state
					| inputsoutput state -> return False
					| otherwise -> checkdeeper state
		| otherwise = return True
	
	iscomputeremote r = remotetype r == remote

	inputsoutput state = not $ M.null $
		M.filter (`elem` outputkeys)
			(computeInputs state)
	
	checkdeeper state =
		flip allM (M.elems (computeInputs state)) $ \inputkey' -> do
			rs <- keyPossibilities inputkey'
			rs' <- avoidCycles (inputkey:outputkeys) inputkey' rs
			return (rs' == rs)

-- Make sure that the compute state exists.
checkKey :: RemoteStateHandle -> Key -> Annex Bool
checkKey rs k = do
	states <- getComputeStatesUnsorted rs k
	if null states
		then giveup "Missing compute state"
		else return True

-- Unsetting the compute state will prevent computing the key.
dropKey :: RemoteStateHandle -> Maybe SafeDropProof -> Key -> Annex ()
dropKey rs _ k = do
	RemoteMetaData _ old <- getCurrentRemoteMetaData rs k
	addRemoteMetaData k rs (modMeta old DelAllMeta)

storeKeyUnsupported :: Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
storeKeyUnsupported _ _ _ _ = giveup "transfer to compute remote not supported; use git-annex addcomputed instead"

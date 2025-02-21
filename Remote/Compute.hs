{- Compute remote.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Compute (
	remote,
	Interface,
	ComputeState(..),
	setComputeState,
	getComputeStates,
	InterfaceEnv,
	interfaceEnv,
	getComputeProgram,
	runComputeProgram,
) where

import Annex.Common
import Types.Remote
import Types.ProposedAccepted
import Types.MetaData
import Types.Creds
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.SpecialRemote.Config
import Annex.UUID
import Annex.Content
import Annex.Tmp
import Logs.MetaData
import Utility.Metered
import Utility.TimeStamp
import Utility.Env
import qualified Git
import qualified Utility.SimpleProtocol as Proto

import Network.HTTP.Types.URI
import Control.Concurrent.STM
import Data.Time.Clock
import Data.Either
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

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = case getComputeProgram rc of
	Left _err -> return Nothing
	Right program -> do
		interface <- liftIO $ newTMVarIO Nothing
		c <- parsedRemoteConfig remote rc
		cst <- remoteCost gc c veryExpensiveRemoteCost
		return $ Just $ mk program interface c cst
  where
	mk program interface c cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyUnsupported
 		, retrieveKeyFile = computeKey rs program interface
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey rs
		, lockContent = Nothing
		, checkPresent = checkKey rs program interface
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
setupInstance _ mu _ c _ = do
	ComputeProgram program <- either giveup return (getComputeProgram c)
	unlessM (liftIO $ inSearchPath program) $
		giveup $ "Cannot find " ++ program ++ " in PATH"
	u <- maybe (liftIO genUUID) return mu
	gitConfigSpecialRemote u c [("compute", "true")]
	return (c, u)

-- The RemoteConfig is allowed to contain fields from the program's
-- interface. That provides defaults for git-annex addcomputed.
computeConfigParser :: RemoteConfig -> Annex RemoteConfigParser
computeConfigParser rc = do
	Interface interface <- case getComputeProgram rc of
		Left _ -> pure $ Interface []
		Right program -> liftIO (getInterface program) >>= return . \case
			Left _ -> Interface []
			Right interface -> interface
	let m = M.fromList $ mapMaybe collectfields interface
	let ininterface f = M.member (Field (fromProposedAccepted f)) m
	return $ RemoteConfigParser
		{ remoteConfigFieldParsers = 
			[ optionalStringParser programField
				(FieldDesc $ "compute program (must start with \"" ++ safetyPrefix ++ "\")")
			]
		, remoteConfigRestPassthrough = Just
			( ininterface
			, M.toList $ M.mapKeys fromField m
			)
		}
  where
	collectfields (InterfaceInput f d) = Just (f, FieldDesc d)
	collectfields (InterfaceOptionalInput f d) = Just (f, FieldDesc d)
	collectfields (InterfaceValue f d) = Just (f, FieldDesc d)
	collectfields (InterfaceOptionalValue f d) = Just (f, FieldDesc d)
	collectfields _ = Nothing

newtype ComputeProgram = ComputeProgram String
	deriving (Show)

getComputeProgram :: RemoteConfig -> Either String ComputeProgram
getComputeProgram c = case fromProposedAccepted <$> M.lookup programField c of
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

type Description = String

newtype Field = Field { fromField :: String }
	deriving (Show, Eq, Ord)

data InterfaceItem
	= InterfaceInput Field Description
	| InterfaceOptionalInput Field Description
	| InterfaceValue Field Description
	| InterfaceOptionalValue Field Description
	| InterfaceOutput Field Description
	| InterfaceReproducible
	deriving (Show, Eq)

-- List order matters, because when displaying the interface to the
-- user, need to display it in the same order as the program
-- does.
data Interface = Interface [InterfaceItem]
	deriving (Show, Eq)

instance Proto.Receivable InterfaceItem where
	parseCommand "INPUT" = Proto.parse2 InterfaceInput
	parseCommand "INPUT?" = Proto.parse2 InterfaceOptionalInput
	parseCommand "VALUE" = Proto.parse2 InterfaceValue
	parseCommand "VALUE?" = Proto.parse2 InterfaceOptionalValue
	parseCommand "OUTPUT" = Proto.parse2 InterfaceOutput
	parseCommand "REPRODUCIBLE" = Proto.parse0 InterfaceReproducible
	parseCommand _ = Proto.parseFail

data ProcessOutput
	= Computing Field FilePath
	| Progress PercentFloat
	deriving (Show, Eq)

instance Proto.Receivable ProcessOutput where
	parseCommand "COMPUTING" = Proto.parse2 Computing
	parseCommand "PROGRESS" = Proto.parse1 Progress
	parseCommand _ = Proto.parseFail

instance Proto.Serializable Field where
	serialize = fromField
	deserialize = Just . Field

newtype PercentFloat = PercentFloat Float
	deriving (Show, Eq)

instance Proto.Serializable PercentFloat where
	serialize (PercentFloat p) = show p
	deserialize s = PercentFloat <$> readMaybe s

getInterfaceCached :: ComputeProgram -> TMVar (Maybe Interface) -> IO (Either String Interface)
getInterfaceCached program iv =
	atomically (takeTMVar iv) >>= \case
		Nothing -> getInterface program >>= \case
			Left err -> do
				atomically $ putTMVar iv Nothing
				return (Left err)
			Right interface -> ret interface
		Just interface -> ret interface
  where
	ret interface = do
		atomically $ putTMVar iv (Just interface)
		return (Right interface)

getInterface :: ComputeProgram -> IO (Either String Interface)
getInterface (ComputeProgram program) = 
	catchMaybeIO (readProcess program ["interface"]) >>= \case
		Nothing -> return $ Left $ "Failed to run " ++ program
		Just output -> return $ case parseInterface output of
			Right i -> Right i
			Left err -> Left $ program ++ " interface output problem: " ++ err

parseInterface :: String -> Either String Interface
parseInterface = go [] . lines
  where
	go is []
		| null is = Left "empty interface output"
		| otherwise = Right (Interface (reverse is))
	go is (l:ls)
		| null l = go is ls
		| otherwise = case Proto.parseMessage l of
			Just i -> go (i:is) ls
			Nothing -> Left $ "Unable to parse line: \"" ++ l ++ "\""

data ComputeInput = ComputeInput Key FilePath
	deriving (Show, Eq)

data ComputeValue = ComputeValue String
	deriving (Show, Eq)

data ComputeOutput = ComputeOutput Key
	deriving (Show, Eq)

data ComputeState = ComputeState
	{ computeInputs :: M.Map Field ComputeInput
	, computeValues :: M.Map Field ComputeValue
	, computeOutputs :: M.Map Field ComputeOutput
	}
	deriving (Show, Eq)

{- Formats a ComputeState as an URL query string.
 -
 - Prefixes fields with "k" and "f" for computeInputs, with
 - "v" for computeValues and "o" for computeOutputs.
 -
 - When the passed Key is an output, rather than duplicate it
 - in the query string, that output has no value.
 -
 - Fields in the query string are sorted. This is in order to ensure
 - that the same ComputeState is always formatted the same way.
 -
 - Example: "ffoo=somefile&kfoo=WORM--foo&oresult&vbar=11"
 -}
formatComputeState :: Key -> ComputeState -> B.ByteString
formatComputeState k st = renderQuery False $ sortOn fst $ concat
	[ concatMap formatinput $ M.toList (computeInputs st)
	, map formatvalue $ M.toList (computeValues st)
	, map formatoutput $ M.toList (computeOutputs st)
	]
  where
	formatinput (f, ComputeInput key file) =
		[ ("k" <> fb, Just (serializeKey' key))
		, ("f" <> fb, Just (toRawFilePath file))
		]
	  where
		fb = encodeBS (fromField f)
	formatvalue (f, ComputeValue v) =
		("v" <> encodeBS (fromField f), Just (encodeBS v))
	formatoutput (f, ComputeOutput key) = 
		("o" <> encodeBS (fromField f),
			if key == k
				then Nothing
				else Just (serializeKey' key)
		)

parseComputeState :: Key -> B.ByteString -> Maybe ComputeState
parseComputeState k b =
	let q = parseQuery b
	    st = go emptycomputestate (M.fromList q) q
	in if st == emptycomputestate then Nothing else Just st
  where
	emptycomputestate = ComputeState mempty mempty mempty
	go c _ [] = c
	go c m ((f, v):rest) = 
		let c' = fromMaybe c $ case decodeBS f of
			('f':f') -> do
				file <- fromRawFilePath <$> v
				kv <- M.lookup (encodeBS ('k':f')) m
				key <- deserializeKey' =<< kv
				Just $ c
					{ computeInputs =
						M.insert (Field f')
							(ComputeInput key file)
							(computeInputs c)
					}
			('v':f') -> do
				val <- decodeBS <$> v
				Just $ c
					{ computeValues = 
						M.insert (Field f')
							(ComputeValue val)
							(computeValues c)
					}
			('o':f') -> case v of
				Just kv -> do
					key <- deserializeKey' kv
					Just $ c
						{ computeOutputs = 
							M.insert (Field f')
								(ComputeOutput key)
								(computeOutputs c)
						}
				Nothing -> Just $ c
					{ computeOutputs =
						M.insert (Field f')
							(ComputeOutput k)
							(computeOutputs c)
					}
			_ -> Nothing
		in go c' m rest

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

getComputeStates :: RemoteStateHandle -> Key -> Annex [(NominalDiffTime, ComputeState)]
getComputeStates rs k = do
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

data InterfaceEnv = InterfaceEnv [(String, Either Key String)]

data InterfaceOutputs = InterfaceOutputs (M.Map Field Key)

{- Finds the first compute state that provides everything required by the
 - interface, and returns a list of what should be provided to the program
 - in its environment, and what outputs the program is expected to make.
 -}
interfaceEnv :: [ComputeState] -> Interface -> Either String (InterfaceEnv, InterfaceOutputs)
interfaceEnv states interface = go Nothing states
  where
	go (Just firsterr) [] = Left firsterr
	go Nothing [] = interfaceEnv' (ComputeState mempty mempty mempty) interface
	go firsterr (state:rest) = case interfaceEnv' state interface of
		Right v -> Right v
		Left e
			| null rest -> Left (fromMaybe e firsterr)
			| otherwise -> go (firsterr <|> Just e) rest

interfaceEnv' :: ComputeState -> Interface -> Either String (InterfaceEnv, InterfaceOutputs)
interfaceEnv' state interface@(Interface i) = 
	case partitionEithers (mapMaybe go i) of
		([], r) -> Right 
			( InterfaceEnv (map (\(f, v) -> (fromField f, v)) r)
			, interfaceOutputs state interface
			)
		(problems, _) -> Left $ unlines problems
  where
	go (InterfaceInput field desc) =
		case M.lookup field (computeInputs state) of
			Just (ComputeInput key _file) -> Just $
				Right (field, Left key)
			Nothing -> Just $
				Left $ "Missing required input \"" ++ fromField field ++ "\" -- " ++ desc
	go (InterfaceOptionalInput field _desc) =
		case M.lookup field (computeInputs state) of
			Just (ComputeInput key _file) -> Just $
				Right (field, Left key)
			Nothing -> Nothing
	go (InterfaceValue field desc) =
		case M.lookup field (computeValues state) of
			Just (ComputeValue v) -> Just $
				Right (field, Right v)
			Nothing -> Just $
				Left $ "Missing required value \"" ++ fromField field ++ "\" -- " ++ desc
	go (InterfaceOptionalValue field _desc) =
		case M.lookup field (computeValues state) of
			Just (ComputeValue v) -> Just $
				Right (field, Right v)
			Nothing -> Nothing
	go (InterfaceOutput _ _) = Nothing
	go InterfaceReproducible = Nothing

interfaceOutputs :: ComputeState -> Interface -> InterfaceOutputs
interfaceOutputs state (Interface interface) =
	InterfaceOutputs $ M.fromList $ mapMaybe go interface
  where
	go (InterfaceOutput field _) = do
		ComputeOutput key <- M.lookup field (computeOutputs state)
		Just (field, key)
	go _ = Nothing

computeProgramEnvironment :: InterfaceEnv -> Annex [(String, String)]
computeProgramEnvironment (InterfaceEnv ienv) = do
	environ <- filter (caninherit . fst) <$> liftIO getEnvironment
	interfaceenv <- mapM go ienv
	return $ environ ++ interfaceenv
  where
	envprefix = "ANNEX_COMPUTE_"
	caninherit v = not (envprefix `isPrefixOf` v)
	go (f, Right v) = return (envprefix ++ f, v)
	go (f, Left k) =
		ifM (inAnnex k)
			( do
				objloc <- calcRepo (gitAnnexLocation k)
				return (envprefix ++ f, fromOsPath objloc)
			, giveup "missing an input to the computation"
			)

runComputeProgram
	:: ComputeProgram
	-> Key
	-> AssociatedFile
	-> OsPath
	-> MeterUpdate
	-> VerifyConfig
	-> (InterfaceEnv, InterfaceOutputs)
	-> Annex Verification
runComputeProgram (ComputeProgram program) k _af dest p vc (ienv, InterfaceOutputs iout) = do
	environ <- computeProgramEnvironment ienv
	withOtherTmp $ \tmpdir -> 
		go environ tmpdir
			`finally` liftIO (removeDirectoryRecursive tmpdir)
  where
	go environ tmpdir = do
		let pr = (proc program [])
			 { cwd = Just $ fromOsPath tmpdir
			 , std_out = CreatePipe
			 , env = Just environ
			 }
		computing <- liftIO $ withCreateProcess pr $
			processoutput mempty tmpdir
		finish computing tmpdir
	
	processoutput computing tmpdir _ (Just h) _ pid =
		hGetLineUntilExitOrEOF pid h >>= \case
			Just l
				| null l -> processoutput computing tmpdir Nothing (Just h) Nothing pid
				| otherwise -> parseoutput computing l >>= \case
					Just computing' -> 
						processoutput computing' tmpdir Nothing (Just h) Nothing pid
					Nothing -> do
						hClose h
						ifM (checkSuccessProcess pid)
							( giveup $ program ++ " output included an unparseable line: \"" ++ l ++ "\""
							, giveup $ program ++ " exited unsuccessfully"
							)
			Nothing -> do
				hClose h
				unlessM (checkSuccessProcess pid) $
					giveup $ program ++ " exited unsuccessfully"
				return computing
	processoutput _ _ _ _ _ _ = error "internal"
	
	parseoutput computing l = case Proto.parseMessage l of
		Just (Computing field file) ->
			case M.lookup field iout of
				Just key -> do
					when (key == k) $
						-- XXX can start watching the file and updating progess now
						return ()
					return $ Just $
						M.insert key (toRawFilePath file) computing
				Nothing -> return (Just computing)
		Just (Progress percent) -> do
			-- XXX
			return Nothing
		Nothing -> return Nothing
	
	finish computing tmpdir = do
		case M.lookup k computing of
			Nothing -> giveup $ program ++ " exited successfully, but failed to output a filename"
			Just file -> do
				let file' = tmpdir </> file
				unlessM (liftIO $ doesFileExist file') $
					giveup $ program ++ " exited sucessfully, but failed to write the computed file"
				catchNonAsync (liftIO $ moveFile file' dest)
					(\err -> giveup $ "failed to move the computed file: " ++ show err)
		
		-- Try to move any other computed object files into the annex.
		forM_ (M.toList computing) $ \(key, file) ->
			when (k /= key) $ do
				let file' = tmpdir </> file
				whenM (liftIO $ doesFileExist file') $
					whenM (verifyKeyContentPostRetrieval RetrievalAllKeysSecure vc verification k file') $
						void $ tryNonAsync $ moveAnnex k file'

		return verification
	
	-- The program might not be reproducible, so require strong
	-- verification.
	verification = MustVerify

computeKey :: RemoteStateHandle -> ComputeProgram -> TMVar (Maybe Interface) -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
computeKey rs program iv k af dest p vc =
	liftIO (getInterfaceCached program iv) >>= \case
		Left err -> giveup err
		Right interface -> do
			states <- map snd . sortOn fst
				<$> getComputeStates rs k
			either giveup (runComputeProgram program k af dest p vc)
				(interfaceEnv states interface)

-- Make sure that the compute state has everything needed by
-- the program's current interface.
checkKey :: RemoteStateHandle -> ComputeProgram -> TMVar (Maybe Interface) -> Key -> Annex Bool
checkKey rs program iv k = do
	states <- map snd <$> getComputeStates rs k
	liftIO (getInterfaceCached program iv) >>= \case
		Left err -> giveup err
		Right interface ->
			case interfaceEnv states interface of
				Right _ -> return True
				Left _ -> return False

-- Unsetting the compute state will prevent computing the key.
dropKey :: RemoteStateHandle -> Maybe SafeDropProof -> Key -> Annex ()
dropKey rs _ k = do
	RemoteMetaData _ old <- getCurrentRemoteMetaData rs k
	addRemoteMetaData k rs (modMeta old DelAllMeta)

storeKeyUnsupported :: Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
storeKeyUnsupported _ _ _ _ = giveup "transfer to compute remote not supported; use git-annex addcomputed instead"

{- Compute remote.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Compute (remote) where

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
import Logs.MetaData
import Utility.Metered
import Utility.Hash
import Utility.TimeStamp
import Git.FilePath
import qualified Git
import qualified Utility.SimpleProtocol as Proto

import Control.Concurrent.STM
import Data.Time.Clock
import Data.Either
import Data.Char
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
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
		Right program -> liftIO (getInterfaceUncached program) >>= return . \case
			Left _ -> Interface []
			Right interface -> interface
	let m = M.fromList $ mapMaybe collectfields interface
	let ininterface f = case toField (fromProposedAccepted f) of
		Just f' -> M.member f' m
		Nothing -> False
	return $ RemoteConfigParser
		{ remoteConfigFieldParsers = 
			[ optionalStringParser programField
				(FieldDesc $ "compute program (must start with \"" ++ safetyPrefix ++ "\")")
			]
		, remoteConfigRestPassthrough = Just (ininterface, M.toList $ M.mapKeys fromField m)
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

newtype Field = Field MetaField
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

instance Proto.Serializable Field where
	serialize = fromField
	deserialize = toField

-- While MetaField is case insensitive, environment variable names are not,
-- so make Field always lower cased.
toField :: String -> Maybe Field
toField f = Field <$> toMetaField (T.pack (map toLower f))

fromField :: Field -> String
fromField (Field f) = T.unpack (fromMetaField f) 

getInterface :: ComputeProgram -> TMVar (Maybe Interface) -> IO (Either String Interface)
getInterface program iv =
	atomically (takeTMVar iv) >>= \case
		Nothing -> getInterfaceUncached program >>= \case
			Left err -> do
				atomically $ putTMVar iv Nothing
				return (Left err)
			Right interface -> ret interface
		Just interface -> ret interface
  where
	ret interface = do
		atomically $ putTMVar iv (Just interface)
		return (Right interface)

getInterfaceUncached :: ComputeProgram -> IO (Either String Interface)
getInterfaceUncached (ComputeProgram program) = 
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
	, computeTimeEstimate :: NominalDiffTime
	}
	deriving (Show, Eq)

-- Generates a hash of a ComputeState.
--
-- This is used as a short unique identifier in the metadata fields,
-- since more than one ComputeState may be stored in the compute remote's
-- metadata for a given Key.
--
-- A md5 is fine for this. It does not need to protect against intentional
-- collisions. And 2^64 is a sufficiently small chance of accidental
-- collision.
hashComputeState :: ComputeState -> String
hashComputeState state = show $ md5s $
	mconcat (map (go goi) (M.toAscList (computeInputs state)))
	<>
	mconcat (map (go gov) (M.toAscList (computeValues state)))
	<>
	mconcat (map (go goo) (M.toAscList (computeOutputs state)))
	<>
	encodeBS (show (computeTimeEstimate state))
  where
	go c (Field f, v) = T.encodeUtf8 (fromMetaField f) <> c v
	goi (ComputeInput k f) = serializeKey' k <> encodeBS f
	gov (ComputeValue s) = encodeBS s
	goo (ComputeOutput k) = serializeKey' k

computeStateMetaData :: ComputeState -> MetaData
computeStateMetaData = undefined

-- FIXME: Need to unswizzle the mixed up metadata based on hash prefixes.
metaDataComputeStates :: MetaData -> [ComputeState]
metaDataComputeStates (MetaData m) =
	go (ComputeState mempty mempty mempty 0) (M.toList m)
  where
	go c ((f,v):rest) = 
		let c' = case T.unpack (fromMetaField f) of
			('i':'n':'p':'u':'t':'-':f') -> case M.lookup m =<< toMetaField (T.pack ("key-" ++ f')) of
				Nothing -> c
				Just kv -> case deserializeKey' (fromMetaValue kv) of
					Just k -> c
						{ computeInputs =
							M.insert (toField f)
								(ComputeInput k (decodeBS (fromMetaValue v)))
								(computeOutputs c)
						}
					Nothing -> c
			('v':'a':'l':'u':'e':'-':f') -> c 
				{ computeValues = 
					M.insert (toField f)
						(ComputeValue (decodeBS (fromMetaValue v)))
						(computeValues c)
				}
			('o':'u':'t':'p':'u':'t':'-':f') ->
				case deserializeKey' (fromMetaValue v) of
					Just k -> c
						{ computeOutputs = 
							M.insert (toField f)
								(ComputeOutput k)
								(computeOutputs c)
						}
					Nothing -> c
			('t':'i':'m':'e':'-':f') ->
				case parsePOSIXTime (fromMetaValue v) of
					Just t -> c { computeTimeEstimate = t }
					Nothing -> c
			_ -> c
		in go c' rest

getComputeStates :: RemoteStateHandle -> Key -> Annex [ComputeState]
getComputeStates rs k = do
	RemoteMetaData _ m <- getCurrentRemoteMetaData rs k
	return (metaDataComputeStates m)

setComputeState :: RemoteStateHandle -> Key -> ComputeState -> Annex ()
setComputeState rs k st = addRemoteMetaData k rs (computeStateMetaData st)

{- Finds the first compute state that provides everything required by the
 - interface, and returns a list of what should be provided to the program
 - in its environment.
 -}
interfaceEnv :: [ComputeState] -> Interface -> Either String [(String, Either Key String)]
interfaceEnv states interface = go Nothing states
  where
	go (Just firsterr) [] = Left firsterr
	go Nothing [] = interfaceEnv' (ComputeState mempty mempty mempty 0) interface
	go firsterr (state:rest) = case interfaceEnv' state interface of
		Right v -> Right v
		Left e
			| null rest -> Left (fromMaybe e firsterr)
			| otherwise -> go (firsterr <|> Just e) rest

interfaceEnv' :: ComputeState -> Interface -> Either String [(String, Either Key String)]
interfaceEnv' state (Interface interface) = 
	case partitionEithers (mapMaybe go interface) of
		([], env) -> Right $
			map (\(f, v) -> (fromField f, v)) env
		(problems, _) -> Left $ unlines problems
  where
	go (InterfaceInput name desc) =
		case M.lookup name (computeInputs state) of
			Just (ComputeInput key _file) -> Just $
				Right (name, Left key)
			Nothing -> Just $
				Left $ "Missing required input \"" ++ fromField name ++ "\" -- " ++ desc
	go (InterfaceOptionalInput name desc) =
		case M.lookup name (computeInputs state) of
			Just (ComputeInput key _file) -> Just $
				Right (name, Left key)
			Nothing -> Nothing
	go (InterfaceValue name desc) =
		case M.lookup name (computeValues state) of
			Just (ComputeValue v) -> Just $
				Right (name, Right v)
			nothing -> Just $
				Left $ "Missing required value \"" ++ fromField name ++ "\" -- " ++ desc
	go (InterfaceOptionalValue name desc) =
		case M.lookup name (computeValues state) of
			Just (ComputeValue v) -> Just $
				Right (name, Right v)
			Nothing -> Nothing
	go (InterfaceOutput _ _) = Nothing
	go InterfaceReproducible = Nothing

computeKey :: RemoteStateHandle -> ComputeProgram -> TMVar (Maybe Interface) -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
computeKey rs program iv k _af dest p vc =
	liftIO (getInterface program iv) >>= \case
		Left err -> giveup err
		Right interface -> do
			states <- sortBy (comparing computeTimeEstimate)
				<$> getComputeStates rs k
			case interfaceEnv states interface of
				Left err -> giveup err
				Right ienv -> undefined -- TODO

-- Make sure that the compute state has everything needed by
-- the program's current interface.
checkKey :: RemoteStateHandle -> ComputeProgram -> TMVar (Maybe Interface) -> Key -> Annex Bool
checkKey rs program iv k = do
	states <- getComputeStates rs k
	liftIO (getInterface program iv) >>= \case
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

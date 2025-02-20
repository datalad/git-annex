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
import Types.Creds
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.SpecialRemote.Config
import Annex.UUID
import Logs.RemoteState
import Utility.Metered
import qualified Git
import qualified Utility.SimpleProtocol as Proto

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S

remote :: RemoteType
remote = RemoteType
	{ typename = "compute"
	, enumerate = const $ findSpecialRemotes "compute"
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser programField
			(FieldDesc $ "compute program (must start with \"" ++ safetyPrefix ++ "\")")
		]
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
 		, retrieveKeyFile = computeKey program interface
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey rs
		, lockContent = Nothing
		, checkPresent = checkKey program interface
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

type Name = String
type Description = String
type Id = String

data InterfaceItem
	= InterfaceInput Id Description
	| InterfaceOptionalInput Id Description
	| InterfaceValue Name Description
	| InterfaceOptionalValue Name Description
	| InterfaceOutput Id Description
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

getInterface :: ComputeProgram -> TMVar (Maybe Interface) -> IO (Either String Interface)
getInterface program iv =
	atomically (takeTMVar iv) >>= \case
		Nothing -> getInterface' program >>= \case
			Left err -> do
				atomically $ putTMVar iv Nothing
				return (Left err)
			Right interface -> ret interface
		Just interface -> ret interface
  where
	ret interface = do
		atomically $ putTMVar iv (Just interface)
		return (Right interface)

getInterface' :: ComputeProgram -> IO (Either String Interface)
getInterface' (ComputeProgram program) = 
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

data ComputeState = ComputeState
	{ computeInputs :: M.Map Id ComputInput
	, computeValues :: M.Map Id ComputeValue
	}
	deriving (Show, Eq)

-- The state is URI encoded.
--
-- A ComputeValue with Id "foo" is represented as "vfoo=value"
-- A ComputeInput with Id "foo" is represented as "kfoo=key&pfoo=path"
formatComputeState :: ComputeState -> String
formatComputeState st = 
	map formatinput (computeInputes st)
	++ concatMap formatvalue (computeValues st) 

parseComputeState :: String -> ComputeState
parseComputeState = 

-- TODO
computeKey :: ComputeProgram -> TMVar (Maybe Interface) -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
computeKey program iv key _af dest p vc =
	liftIO (getInterface program iv) >>= \case
		Left err -> giveup err
		Right interface -> undefined

-- TODO Make sure that the remote state meets the program's current
-- interface.
checkKey :: ComputeProgram -> TMVar (Maybe Interface) -> Key -> Annex Bool
checkKey program iv _ =
	liftIO (getInterface program iv) >>= \case
		Left err -> giveup err
		Right interface -> undefined

-- Removing remote state will prevent computing the key.
dropKey :: RemoteStateHandle -> Maybe SafeDropProof -> Key -> Annex ()
dropKey rs _ k = setRemoteState rs k mempty

storeKeyUnsupported :: Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
storeKeyUnsupported _ _ _ _ = giveup "transfer to compute remote not supported; use git-annex addcomputed instead"


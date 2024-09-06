{- git-annex simulator
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Sim where

import Annex.Common
import Utility.DataUnits
import Types.NumCopies
import Types.Group
import Types.StandardGroups
import Types.TrustLevel
import Git.Types
import Git
import Backend.Hash (genTestKey)
import Annex.UUID
import Annex.FileMatcher
import Logs.Group
import Logs.Trust
import Logs.PreferredContent
import Logs.Remote
import Logs.MaxSize
import qualified Remote

import System.Random
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.UUID as U
import qualified Data.UUID.V5 as U5

-- Runs the simulation one step. As well as the updated SimState,
-- returns SimCommands for every change that the simulation made.
-- Eg, CommandPresent is returned when a file's content is added to a repo,
-- and CommandNotPresent when a file's content is dropped.
stepSimulation :: SimState -> (SimState, [SimCommand])
stepSimulation st = undefined -- XXX TODO

data SimState = SimState
	{ simRepos :: M.Map RepoName UUID
	, simRepoState :: M.Map RepoName SimRepoState
	, simConnections :: M.Map RepoName (S.Set RepoName)
	, simFiles :: M.Map FilePath Key
	, simRng :: StdGen
	, simTrustLevels :: M.Map RepoName TrustLevel
	, simNumCopies :: NumCopies
	, simGroups :: M.Map RepoName (S.Set Group)
	, simWanted :: M.Map RepoName PreferredContentExpression
	, simRequired :: M.Map RepoName PreferredContentExpression
	, simGroupWanted :: M.Map Group PreferredContentExpression
	, simMaxSize :: M.Map RepoName MaxSize
	, simRebalance :: Bool
	, simGetExistingRepoByName :: GetExistingRepoByName
	}
	deriving (Show)

emptySimState :: Int -> GetExistingRepoByName -> SimState
emptySimState rngseed repobyname = SimState
	{ simRepos = mempty
	, simRepoState = mempty
	, simConnections = mempty
	, simFiles = mempty
	, simRng = mkStdGen rngseed
	, simTrustLevels = mempty
	, simNumCopies = configuredNumCopies 1
	, simGroups = mempty
	, simWanted = mempty
	, simRequired = mempty
	, simGroupWanted = mempty
	, simMaxSize = mempty
	, simRebalance = False
	, simGetExistingRepoByName = repobyname
	}

-- State that can vary between different repos in the simulation.
data SimRepoState = SimRepoState
	{ simLocations :: M.Map Key (S.Set RepoName)
	, simIsSpecialRemote :: Bool
	}
	deriving (Show, Eq)

setPresentKey :: RepoName -> Key -> SimRepoState -> SimRepoState
setPresentKey repo k rst = rst
	{ simLocations = 
		M.insertWith S.union k (S.singleton repo) (simLocations rst)
	}

newtype RepoName = RepoName { fromRepoName :: String }
	deriving (Show, Eq, Ord)

data SimCommand
	= CommandInit RepoName
	| CommandInitRemote RepoName
	| CommandUse RepoName String
	| CommandConnect RepoName RepoName
	| CommandDisconnect RepoName RepoName
	| CommandAddTree RepoName PreferredContentExpression
	| CommandAdd FilePath ByteSize RepoName
	| CommandStep Int
	| CommandSeed Int
	| CommandPresent RepoName FilePath
	| CommandNotPresent RepoName FilePath
	| CommandNumCopies Int
	| CommandTrustLevel RepoName String
	| CommandGroup RepoName Group
	| CommandUngroup RepoName Group
	| CommandWanted RepoName PreferredContentExpression
	| CommandRequired RepoName PreferredContentExpression
	| CommandGroupWanted Group PreferredContentExpression
	| CommandMaxSize RepoName MaxSize
	| CommandRebalance Bool
	deriving (Show)

applySimCommand
	:: SimCommand
	-> SimState
	-> Either String (Either (Annex SimState) SimState)
applySimCommand (CommandInit reponame) st =
	checkNonexistantRepo reponame st $
		let (u, st') = genSimUUID st reponame
		in Right $ Right $ addRepo reponame (newSimRepoConfig u False) st'
applySimCommand (CommandInitRemote reponame) st = 
	checkNonexistantRepo reponame st $
		let (u, st') = genSimUUID st reponame
		in Right $ Right $ addRepo reponame (newSimRepoConfig u True) st'
applySimCommand (CommandUse reponame s) st =
	case getExistingRepoByName (simGetExistingRepoByName st) s of
		Right existingrepo -> checkNonexistantRepo reponame st $
			Right $ Right $ addRepo reponame existingrepo st
		Left msg -> Left $ "Unable to use a repository \"" 
			++ fromRepoName reponame 
			++ "\" in the simulation because " ++ msg
applySimCommand (CommandConnect repo remote) st = 
	checkKnownRepo repo st $ checkKnownRepo remote st $ Right $ Right $ st
		{ simConnections = 
			let s = case M.lookup repo (simConnections st) of
				Just cs -> S.insert remote cs
				Nothing -> S.singleton remote
			in M.insert repo s (simConnections st)
		}
applySimCommand (CommandDisconnect repo remote) st = 
	checkKnownRepo repo st $ checkKnownRepo remote st $ Right $ Right $ st
		{ simConnections = 
			let sc = case M.lookup repo (simConnections st) of
				Just s -> S.delete remote s
				Nothing -> S.empty
			in M.insert repo sc (simConnections st)
		}
applySimCommand (CommandAddTree repo expr) st =
	checkKnownRepo repo st $
		checkValidPreferredContentExpression expr $ Left $
			error "TODO" -- XXX
applySimCommand (CommandAdd file sz repo) st = checkKnownRepo repo st $
	let (k, st') = genSimKey sz st
	in Right $ Right $ st'
		{ simFiles = M.insert file k (simFiles st')
		, simRepoState = case M.lookup repo (simRepoState st') of
			Just rst -> M.insert repo
				(setPresentKey repo k rst)
				(simRepoState st')
			Nothing -> error "no simRepoState in applySimCommand CommandAdd"
		}
applySimCommand (CommandStep n) st
	| n > 0 = applySimCommand
		(CommandStep (pred n))
		(fst $ stepSimulation st)
	| otherwise = Right $ Right st
applySimCommand (CommandSeed rngseed) st = Right $ Right $ st
	{ simRng = mkStdGen rngseed
	}
applySimCommand (CommandPresent repo file) st = checkKnownRepo repo st $
	case (M.lookup file (simFiles st), M.lookup repo (simRepoState st)) of
		(Just k, Just rst) -> case M.lookup k (simLocations rst) of
			Just locs | S.member repo locs -> Right $ Right st
			_ -> missing
		(Just _, Nothing) -> missing
		(Nothing, _) -> Left $ "Expected " ++ file
			++ " to be present in " ++ fromRepoName repo 
			++ ", but the simulation does not include that file."
  where
	missing = Left $ "Expected " ++ file ++ " to be present in " 
		++ fromRepoName repo ++ ", but it is not."
applySimCommand (CommandNotPresent repo file) st = checkKnownRepo repo st $
	case (M.lookup file (simFiles st), M.lookup repo (simRepoState st)) of
		(Just k, Just rst) -> case M.lookup k (simLocations rst) of
			Just locs | S.notMember repo locs -> Right $ Right st
			_ -> present
		(Just _, Nothing) -> present
		(Nothing, _) -> Left $ "Expected " ++ file
			++ " to not be present in " ++ fromRepoName repo 
			++ ", but the simulation does not include that file."
  where
	present = Left $ "Expected " ++ file ++ " not to be present in " 
		++ fromRepoName repo ++ ", but it is present."
applySimCommand (CommandNumCopies n) st = Right $ Right $ st
	{ simNumCopies = configuredNumCopies n
	}
applySimCommand (CommandTrustLevel repo s) st = checkKnownRepo repo st $
	case readTrustLevel s of
		Just trustlevel -> Right $ Right $ st
			{ simTrustLevels = M.insert repo trustlevel
				(simTrustLevels st)
			}
		Nothing -> Left $ "Unknown trust level \"" ++ s ++ "\"."
applySimCommand (CommandGroup repo groupname) st = checkKnownRepo repo st $
	Right $ Right $ st
		{ simGroups = M.insertWith S.union repo
			(S.singleton groupname)
			(simGroups st)
		}
applySimCommand (CommandUngroup repo groupname) st = checkKnownRepo repo st $
	Right $ Right $ st
		{ simGroups = M.adjust (S.delete groupname) repo (simGroups st)
		}
applySimCommand (CommandWanted repo expr) st = checkKnownRepo repo st $
	checkValidPreferredContentExpression expr $ Right $ st
		{ simWanted = M.insert repo expr (simWanted st)
		}
applySimCommand (CommandRequired repo expr) st = checkKnownRepo repo st $
	checkValidPreferredContentExpression expr $ Right $ st
		{ simRequired = M.insert repo expr (simRequired st)
		}
applySimCommand (CommandGroupWanted groupname expr) st =
	checkValidPreferredContentExpression expr $ Right $ st
		{ simGroupWanted = M.insert groupname expr (simGroupWanted st)
		}
applySimCommand (CommandMaxSize repo sz) st = checkKnownRepo repo st $
	Right $ Right $ st
		{ simMaxSize = M.insert repo sz (simMaxSize st)
		}
applySimCommand (CommandRebalance b) st = Right $ Right $ st
	{ simRebalance = b
	}

checkNonexistantRepo :: RepoName -> SimState -> Either String a -> Either String a
checkNonexistantRepo reponame st a = case M.lookup reponame (simRepos st) of
	Nothing -> a
	Just _ -> Left $ "There is already a repository in the simulation named \""
		++ fromRepoName reponame ++ "\"."

checkKnownRepo :: RepoName -> SimState -> Either String a -> Either String a
checkKnownRepo reponame st a = case M.lookup reponame (simRepos st) of
	Just _ -> a
	Nothing -> Left $ "No repository in the simulation is named \""
		++ fromRepoName reponame ++ "\"."

checkValidPreferredContentExpression :: PreferredContentExpression -> v -> Either String v
checkValidPreferredContentExpression expr v =
	case checkPreferredContentExpression expr of
		Nothing -> Right v
		Just e -> Left $ "Failed parsing \"" ++ expr ++ "\": " ++ e

simRandom :: SimState -> (StdGen -> (v, StdGen)) -> (v -> r) -> (r, SimState)
simRandom st mk f =
	let (v, rng) = mk (simRng st)
	in (f v, st { simRng = rng })

randomWords :: Int -> StdGen -> ([Word8], StdGen)
randomWords = go []
  where
	go c n g
		| n < 1 = (c, g)
		| otherwise = 
			let (w, g') = random g
			in go (w:c) (pred n) g'

genSimKey :: ByteSize -> SimState -> (Key, SimState)
genSimKey sz st = simRandom st (randomWords 1024) mk
  where
	mk b =
		let tk = genTestKey $ L.pack b
		in alterKey tk $ \kd -> kd { keySize = Just sz }

genSimUUID :: SimState -> RepoName -> (UUID, SimState)
genSimUUID st (RepoName reponame) = simRandom st (randomWords 1024)
	(\l -> genUUIDInNameSpace simUUIDNameSpace (encodeBS reponame <> B.pack l))

simUUIDNameSpace :: U.UUID
simUUIDNameSpace = U5.generateNamed U5.namespaceURL $
        B.unpack "http://git-annex.branchable.com/git-annex-sim/"

newtype GetExistingRepoByName = GetExistingRepoByName 
	{ getExistingRepoByName :: String -> Either String SimRepoConfig
	}

instance Show GetExistingRepoByName where
	show _ = "GetExistingRepoByName"

data SimRepoConfig = SimRepoConfig
	{ simRepoUUID :: UUID
	, simRepoIsSpecialRemote :: Bool
	, simRepoGroups :: S.Set Group
	, simRepoTrustLevel :: TrustLevel
	, simRepoPreferredContent :: Maybe PreferredContentExpression
	, simRepoRequiredContent :: Maybe PreferredContentExpression
	, simRepoGroupPreferredContent :: M.Map Group PreferredContentExpression
	, simRepoMaxSize :: Maybe MaxSize
	}
	deriving (Show)

newSimRepoConfig :: UUID -> Bool -> SimRepoConfig
newSimRepoConfig u isspecialremote = SimRepoConfig
	{ simRepoUUID = u 
	, simRepoIsSpecialRemote = isspecialremote
	, simRepoGroups = mempty
	, simRepoTrustLevel = def
	, simRepoPreferredContent = Nothing
	, simRepoRequiredContent = Nothing
	, simRepoGroupPreferredContent = mempty
	, simRepoMaxSize = Nothing
	}

addRepo :: RepoName -> SimRepoConfig -> SimState -> SimState
addRepo reponame simrepo st = st
	{ simRepos = M.insert reponame (simRepoUUID simrepo) (simRepos st)
	, simRepoState = M.insert reponame rst (simRepoState st)
	, simConnections = M.insert reponame mempty (simConnections st)
	, simGroups = M.insert reponame (simRepoGroups simrepo) (simGroups st)
	, simTrustLevels = M.insert reponame
		(simRepoTrustLevel simrepo)
		(simTrustLevels st)
	, simWanted = M.alter
		(const $ simRepoPreferredContent simrepo)
		reponame 
		(simWanted st)
	, simRequired = M.alter
		(const $ simRepoRequiredContent simrepo)
		reponame 
		(simRequired st)
	, simGroupWanted = M.union 
		(simRepoGroupPreferredContent simrepo)
		(simGroupWanted st)
	, simMaxSize = M.alter
		(const $ simRepoMaxSize simrepo)
		reponame
		(simMaxSize st)
	}
  where
	rst = SimRepoState
		{ simLocations = mempty
		, simIsSpecialRemote = simRepoIsSpecialRemote simrepo
		}

mkGetExistingRepoByName :: Annex GetExistingRepoByName
mkGetExistingRepoByName = do
	groupmap <- groupMap
	trustmap <- trustMap
	pcmap <- preferredContentMapRaw
	rcmap <- requiredContentMapRaw
	gpcmap <- groupPreferredContentMapRaw
	maxsizes <- getMaxSizes
	nametouuid <- Remote.nameToUUID''
	remoteconfigmap <- readRemoteLog
	return $ GetExistingRepoByName $ \name -> 
		case nametouuid name of
			(u:[], _) -> Right $ 
				let gs = fromMaybe S.empty $
					M.lookup u (groupsByUUID groupmap)
				in SimRepoConfig
					{ simRepoUUID = u
					, simRepoIsSpecialRemote =
						M.member u remoteconfigmap
					, simRepoGroups = gs
					, simRepoTrustLevel =
						lookupTrust' u trustmap
					, simRepoPreferredContent =
						M.lookup u pcmap
					, simRepoRequiredContent =
						M.lookup u rcmap
					, simRepoGroupPreferredContent =
						M.restrictKeys gpcmap gs
					, simRepoMaxSize =
						M.lookup u maxsizes
					}
			(_, msg) -> Left msg

cloneSimRepo :: RepoName -> UUID -> Repo -> FilePath -> IO ()
cloneSimRepo simreponame u parent dest = do
	cloned <- boolSystem "git" 
		[ Param "clone"
		, Param "--shared"
		, Param "--quiet"
		-- Avoid overhead of checking out the working tree.
		-- Note that, on visiting the simulated repo,
		-- the working tree needs to be reset.
		, Param "--no-checkout"
		, File (fromRawFilePath (repoPath parent))
		, File dest
		]
	unless cloned $ giveup "git clone failed"
	-- TODO delete origin remote from clone, to avoid foot-shooting

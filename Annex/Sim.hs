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
import Types.Difference
import Git
import Backend.Hash (genTestKey)
import Annex.UUID
import Annex.FileMatcher
import Annex.Init
import Annex.Startup
import Annex.Link
import Annex.Wanted
import Logs.Group
import Logs.Trust
import Logs.PreferredContent
import Logs.NumCopies
import Logs.Remote
import Logs.MaxSize
import Logs.Difference
import Logs.UUID
import Logs.Location
import qualified Annex
import qualified Remote
import qualified Git.Construct
import qualified Annex.Queue

import System.Random
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.UUID as U
import qualified Data.UUID.V5 as U5
import qualified Utility.RawFilePath as R
import qualified System.FilePath.ByteString as P

data SimState = SimState
	{ simRepos :: M.Map RepoName UUID
	, simRepoList :: [RepoName]
	, simRepoState :: M.Map UUID SimRepoState
	, simConnections :: M.Map UUID (S.Set RemoteName)
	, simFiles :: M.Map RawFilePath Key
	, simRng :: StdGen
	, simTrustLevels :: M.Map UUID TrustLevel
	, simNumCopies :: NumCopies
	, simMinCopies :: MinCopies
	, simGroups :: M.Map UUID (S.Set Group)
	, simWanted :: M.Map UUID PreferredContentExpression
	, simRequired :: M.Map UUID PreferredContentExpression
	, simGroupWanted :: M.Map Group PreferredContentExpression
	, simMaxSize :: M.Map UUID MaxSize
	, simRebalance :: Bool
	, simGetExistingRepoByName :: GetExistingRepoByName
	, simGetSimRepoPath :: GetSimRepoPath
	, simHistory :: [SimCommand]
	, simVectorClock :: VectorClock
	}
	deriving (Show)

emptySimState :: StdGen -> GetExistingRepoByName -> GetSimRepoPath -> SimState
emptySimState rng repobyname getpath = SimState
	{ simRepos = mempty
	, simRepoList = mempty
	, simRepoState = mempty
	, simConnections = mempty
	, simFiles = mempty
	, simRng = rng
	, simTrustLevels = mempty
	, simNumCopies = configuredNumCopies 1
	, simMinCopies = configuredMinCopies 1
	, simGroups = mempty
	, simWanted = mempty
	, simRequired = mempty
	, simGroupWanted = mempty
	, simMaxSize = mempty
	, simRebalance = False
	, simGetExistingRepoByName = repobyname
	, simGetSimRepoPath = getpath
	, simHistory = []
	, simVectorClock = VectorClock 0
	}

-- State that can vary between different repos in the simulation.
data SimRepoState = SimRepoState
	{ simLocations :: M.Map Key (M.Map UUID LocationState)
	, simIsSpecialRemote :: Bool
	, simRepo :: Maybe SimRepo
	, simRepoName :: RepoName
	}
	deriving (Show)

data LocationState = LocationState VectorClock Bool
	deriving (Eq, Show)

newtype VectorClock = VectorClock Int
	deriving (Eq, Ord, Show)

newerLocationState ::  LocationState -> LocationState -> LocationState
newerLocationState l1@(LocationState vc1 _) l2@(LocationState vc2 _)
	| vc1 > vc2 = l1
	| otherwise = l2
		
{- Updates the state of repou to indicate that a key is
 - present or not in u. -}
setPresentKey :: UUID -> Key -> UUID -> SimState -> SimState
setPresentKey u k repou st = st
	{ simRepoState = case M.lookup repou (simRepoState st) of
		Just rst -> M.insert repou
			(setPresentKey' (simVectorClock st) u k rst)
			(simRepoState st)
		Nothing -> error "no simRepoState in setPresentKey"
	}

setPresentKey' :: VectorClock -> UUID -> Key -> SimRepoState -> SimRepoState
setPresentKey' vc u k rst = rst
	{ simLocations = 
		M.insertWith (M.unionWith newerLocationState) k
			(M.singleton u (LocationState vc True))
			(simLocations rst)
	}

getSimLocations :: SimRepoState -> Key -> S.Set UUID
getSimLocations rst k =
	maybe mempty getSimLocations' $
		M.lookup k (simLocations rst)

getSimLocations' :: M.Map UUID LocationState -> S.Set UUID
getSimLocations' = M.keysSet . M.filter present
  where
	present (LocationState _ b) = b

addHistory :: SimState -> SimCommand -> SimState
addHistory st c = st { simHistory = c : simHistory st }

newtype RepoName = RepoName { fromRepoName :: String }
	deriving (Show, Eq, Ord)

newtype RemoteName = RemoteName { fromRemoteName :: String }
	deriving (Show, Eq, Ord)

remoteNameToRepoName :: RemoteName -> RepoName
remoteNameToRepoName (RemoteName n) = RepoName n

repoNameToRemoteName :: RepoName -> RemoteName
repoNameToRemoteName (RepoName n) = RemoteName n

data Connections
	= RepoName :-> RemoteName
	| RemoteName :<- RepoName
	| RepoName :<-> RepoName
	| RepoName :=> Connections
	| RemoteName :<= Connections
	| RepoName :<=> Connections
	deriving (Show)

leftSideOfConnection :: Connections -> RepoName
leftSideOfConnection (reponame :-> _) = reponame
leftSideOfConnection (remotename :<- _) = remoteNameToRepoName remotename
leftSideOfConnection (reponame :<-> _) = reponame
leftSideOfConnection (reponame :=> _) = reponame
leftSideOfConnection (remotename :<= _) = remoteNameToRepoName remotename
leftSideOfConnection (reponame :<=> _) = reponame

getConnection :: Connections -> (RepoName, RemoteName, Maybe Connections)
getConnection (reponame :-> remotename) = (reponame, remotename, Nothing)
getConnection (remotename :<- reponame) = (reponame, remotename, Nothing)
getConnection (reponame1 :<-> reponame2) =
	( reponame1
	, repoNameToRemoteName reponame2
	, Just (reponame2 :-> repoNameToRemoteName reponame1)
	)
getConnection (reponame :=> c) =
	(reponame, repoNameToRemoteName (leftSideOfConnection c), Just c)
getConnection (remotename :<= c) = (leftSideOfConnection c, remotename, Just c)
getConnection (reponame :<=> c) = 
	( reponame
	, repoNameToRemoteName (leftSideOfConnection c)
	, Just (reponame :=> c)
	)

data SimCommand
	= CommandInit RepoName
	| CommandInitRemote RepoName
	| CommandUse RepoName String
	| CommandConnect Connections
	| CommandDisconnect Connections
	| CommandAddTree RepoName PreferredContentExpression
	| CommandAdd RawFilePath ByteSize [RepoName]
	| CommandStep Int
	| CommandAction RepoName SimAction
	| CommandSeed Int
	| CommandPresent RepoName RawFilePath
	| CommandNotPresent RepoName RawFilePath
	| CommandNumCopies Int
	| CommandMinCopies Int
	| CommandTrustLevel RepoName TrustLevel
	| CommandGroup RepoName Group
	| CommandUngroup RepoName Group
	| CommandWanted RepoName PreferredContentExpression
	| CommandRequired RepoName PreferredContentExpression
	| CommandGroupWanted Group PreferredContentExpression
	| CommandMaxSize RepoName MaxSize
	| CommandRebalance Bool
	| CommandComment String
	| CommandBlank
	deriving (Show)

data SimAction
	= ActionPull RemoteName
	| ActionPush RemoteName
	| ActionGetWanted RemoteName
	| ActionDropUnwanted (Maybe RemoteName)
	| ActionSendWanted RemoteName
	| ActionGitPush RemoteName
	| ActionGitPull RemoteName
	deriving (Show)

runSimCommand :: SimCommand -> SimState -> Annex SimState
runSimCommand (CommandStep n) st
	| n > 0 = case randomRepo st of
		(Just (repo, u), st') ->
			let (act, st'') = randomAction u st'
			in runSimCommand (CommandAction repo act) st''
				>>= runSimCommand (CommandStep (pred n))
		(Nothing, st') -> return st'
	| otherwise = return st
runSimCommand cmd st = case applySimCommand cmd st of
	Left err -> giveup err
	Right (Right st') -> return st'
	Right (Left mkst) -> mkst

applySimCommand
	:: SimCommand
	-> SimState
	-> Either String (Either (Annex SimState) SimState)
applySimCommand cmd st = 
	applySimCommand' cmd $ flip addHistory cmd $ st
		{ simVectorClock = 
			let (VectorClock clk) = simVectorClock st 
			in VectorClock (succ clk)
		}

applySimCommand'
	:: SimCommand
	-> SimState
	-> Either String (Either (Annex SimState) SimState)
applySimCommand' (CommandInit reponame) st =
	checkNonexistantRepo reponame st $
		let (u, st') = genSimUUID st reponame
		in Right $ Right $ addRepo reponame (newSimRepoConfig u False) st'
applySimCommand' (CommandInitRemote reponame) st = 
	checkNonexistantRepo reponame st $
		let (u, st') = genSimUUID st reponame
		in Right $ Right $ addRepo reponame (newSimRepoConfig u True) st'
applySimCommand' (CommandUse reponame s) st =
	case getExistingRepoByName (simGetExistingRepoByName st) s of
		Right existingrepo -> checkNonexistantRepo reponame st $
			Right $ Right $ addRepo reponame existingrepo st
		Left msg -> Left $ "Unable to use a repository \"" 
			++ fromRepoName reponame 
			++ "\" in the simulation because " ++ msg
applySimCommand' (CommandConnect connections) st =
	let (repo, remote, mconnections) = getConnection connections
	in checkKnownRepo repo st $ \u -> 
		let st' = st
			{ simConnections = 
				let s = case M.lookup u (simConnections st) of
					Just cs -> S.insert remote cs
					Nothing -> S.singleton remote
				in M.insert u s (simConnections st)
			}
		in case mconnections of
			Nothing -> Right $ Right st'
			Just connections' ->
				applySimCommand' (CommandConnect connections') st'
applySimCommand' (CommandDisconnect connections) st = 
	let (repo, remote, mconnections) = getConnection connections
	in checkKnownRepo repo st $ \u -> 
		let st' = st
			{ simConnections = 
				let sc = case M.lookup u (simConnections st) of
					Just s -> S.delete remote s
					Nothing -> S.empty
				in M.insert u sc (simConnections st)
			}
		in case mconnections of
			Nothing -> Right $ Right $ st
			Just connections' ->
				applySimCommand' (CommandDisconnect connections') st'
applySimCommand' (CommandAddTree repo expr) st =
	checkKnownRepo repo st $ const $
		checkValidPreferredContentExpression expr $ Left $
			error "TODO" -- XXX
applySimCommand' (CommandAdd file sz repos) st = 
	let (k, st') = genSimKey sz st
	in go k st' repos
  where
	go k st' [] = Right $ Right st
	go k st' (repo:rest) = checkKnownRepo repo st' $ \u ->
		let st'' = setPresentKey u k u $ st'
			{ simFiles = M.insert file k (simFiles st')
			}
		in go k st'' rest
applySimCommand' (CommandStep _) _ = error "applySimCommand' CommandStep"
applySimCommand' (CommandAction repo act) st =
	checkKnownRepo repo st $ \u -> 
		applySimAction repo u act st
applySimCommand' (CommandSeed rngseed) st = Right $ Right $ st
	{ simRng = mkStdGen rngseed
	}
applySimCommand' (CommandPresent repo file) st = checkKnownRepo repo st $ \u ->
	case (M.lookup file (simFiles st), M.lookup u (simRepoState st)) of
		(Just k, Just rst)
			| u `S.member` getSimLocations rst k ->
				Right $ Right st
			| otherwise -> missing
		(Just _, Nothing) -> missing
		(Nothing, _) -> Left $ "Expected " ++ fromRawFilePath file
			++ " to be present in " ++ fromRepoName repo 
			++ ", but the simulation does not include that file."
  where
	missing = Left $ "Expected " ++ fromRawFilePath file
		++ " to be present in " 
		++ fromRepoName repo ++ ", but it is not."
applySimCommand' (CommandNotPresent repo file) st = checkKnownRepo repo st $ \u ->
	case (M.lookup file (simFiles st), M.lookup u (simRepoState st)) of
		(Just k, Just rst)
			| u `S.notMember` getSimLocations rst k ->
				Right $ Right st
			| otherwise -> present
		(Just _, Nothing) -> present
		(Nothing, _) -> Left $ "Expected " ++ fromRawFilePath file
			++ " to not be present in " ++ fromRepoName repo 
			++ ", but the simulation does not include that file."
  where
	present = Left $ "Expected " ++ fromRawFilePath file 
		++ " not to be present in " 
		++ fromRepoName repo ++ ", but it is present."
applySimCommand' (CommandNumCopies n) st = Right $ Right $ st
	{ simNumCopies = configuredNumCopies n
	}
applySimCommand' (CommandMinCopies n) st = Right $ Right $ st
	{ simMinCopies = configuredMinCopies n
	}
applySimCommand' (CommandTrustLevel repo trustlevel) st =
	checkKnownRepo repo st $ \u ->
		 Right $ Right $ st
			{ simTrustLevels = M.insert u trustlevel
				(simTrustLevels st)
			}
applySimCommand' (CommandGroup repo groupname) st = checkKnownRepo repo st $ \u ->
	Right $ Right $ st
		{ simGroups = M.insertWith S.union u
			(S.singleton groupname)
			(simGroups st)
		}
applySimCommand' (CommandUngroup repo groupname) st = checkKnownRepo repo st $ \u ->
	Right $ Right $ st
		{ simGroups = M.adjust (S.delete groupname) u (simGroups st)
		}
applySimCommand' (CommandWanted repo expr) st = checkKnownRepo repo st $ \u ->
	checkValidPreferredContentExpression expr $ Right $ st
		{ simWanted = M.insert u expr (simWanted st)
		}
applySimCommand' (CommandRequired repo expr) st = checkKnownRepo repo st $ \u ->
	checkValidPreferredContentExpression expr $ Right $ st
		{ simRequired = M.insert u expr (simRequired st)
		}
applySimCommand' (CommandGroupWanted groupname expr) st =
	checkValidPreferredContentExpression expr $ Right $ st
		{ simGroupWanted = M.insert groupname expr (simGroupWanted st)
		}
applySimCommand' (CommandMaxSize repo sz) st = checkKnownRepo repo st $ \u ->
	Right $ Right $ st
		{ simMaxSize = M.insert u sz (simMaxSize st)
		}
applySimCommand' (CommandRebalance b) st = Right $ Right $ st
	{ simRebalance = b
	}
applySimCommand' (CommandComment _) st = Right $ Right st
applySimCommand' CommandBlank st = Right $ Right st

applySimAction
	:: RepoName
	-> UUID
	-> SimAction
	-> SimState 
	-> Either String (Either (Annex SimState) SimState)
applySimAction r u (ActionPull remote) st = undefined -- TODO
applySimAction r u (ActionPush remote) st = undefined -- TODO
applySimAction r u (ActionGetWanted remote) st =
	overFilesRemote r u remote S.member wanted go st
  where
	wanted k f _ = wantGet NoLiveUpdate False k f
	go u _ f k r st' = setPresentKey u k u $
		addHistory st' $ CommandPresent r f
applySimAction r u (ActionSendWanted remote) st = 
	overFilesRemote r u remote S.notMember wanted go st
  where
	wanted = wantGetBy NoLiveUpdate False
	go _ remoteu f k r st' = 
		-- Sending to a remote updates the location log
		-- of both the repository sending and the remote.
		setPresentKey remoteu k remoteu $
		setPresentKey remoteu k u $
		addHistory st' $ CommandPresent (remoteNameToRepoName remote) f
applySimAction r u (ActionDropUnwanted Nothing) st = undefined -- TODO
applySimAction r u (ActionDropUnwanted (Just remote)) st = undefined -- TODO
applySimAction r u (ActionGitPush remote) st =
	checkKnownRemote remote r u st $ \_ ->
		simulateGitAnnexMerge r (remoteNameToRepoName remote) st
applySimAction r u (ActionGitPull remote) st =
	checkKnownRemote remote r u st $ \_ ->
		simulateGitAnnexMerge (remoteNameToRepoName remote) r st

overFilesRemote
	:: RepoName
	-> UUID
	-> RemoteName
	-> (UUID -> S.Set UUID -> Bool) 
	-> (Maybe Key -> AssociatedFile -> UUID -> Annex Bool)
        -> (UUID -> UUID -> RawFilePath -> Key -> RepoName -> SimState -> SimState)
	-> SimState
	-> Either String (Either (Annex SimState) SimState)
overFilesRemote r u remote remotepred checkwant handlewanted st = 
	checkKnownRemote remote r u st $ \remoteu ->
		Right $ Left $ liftIO $
			runSimRepo u st $ \rst ->
				let l = M.toList $
					M.filter (checkremotepred remoteu rst) $
					simFiles st
				in go remoteu l st
  where
	go _ [] st' = return st'
	go remoteu ((f, k):rest) st' = do
		ifM (checkwant (Just k) af remoteu)
			( go remoteu rest $ handlewanted u remoteu f k r st'
			, go remoteu rest st'
			)
	  where
		af = AssociatedFile $ Just f

	checkremotepred remoteu rst k =
		remotepred remoteu (getSimLocations rst k)

simulateGitAnnexMerge
	:: RepoName
	-> RepoName
	-> SimState
	-> Either String (Either (Annex SimState) SimState)
simulateGitAnnexMerge src dest st = 
	case (M.lookup src (simRepos st), M.lookup dest (simRepos st)) of
		(Just srcu, Just destu) -> case M.lookup destu (simRepoState st) of
			Nothing -> Left $ "Unable to find simRepoState for " ++ fromRepoName dest
			Just destst -> case M.lookup srcu (simRepoState st) of
				Nothing -> Left $ "Unable to find simRepoState for " ++ fromRepoName src
				Just srcst -> Right $ Right $
					let locs = M.unionWith
						(M.unionWith newerLocationState)
						(simLocations destst)
						(simLocations srcst)
					    destst' = destst { simLocations = locs }
					in st
						{ simRepoState = M.insert destu
							destst'
							(simRepoState st)
						}
		_ -> Left $ "Unable to find " ++ fromRepoName src ++ " or " ++ fromRepoName dest ++ " in simRepos"

checkNonexistantRepo :: RepoName -> SimState -> Either String a -> Either String a
checkNonexistantRepo reponame st a = case M.lookup reponame (simRepos st) of
	Nothing -> a
	Just _ -> Left $ "There is already a repository in the simulation named \""
		++ fromRepoName reponame ++ "\"."

checkKnownRepo :: RepoName -> SimState -> (UUID -> Either String a) -> Either String a
checkKnownRepo reponame st a = case M.lookup reponame (simRepos st) of
	Just u -> a u
	Nothing -> Left $ "No repository in the simulation is named \""
		++ fromRepoName reponame ++ "\"."

checkKnownRemote :: RemoteName -> RepoName -> UUID -> SimState -> (UUID -> Either String a) -> Either String a
checkKnownRemote remotename reponame u st a =
	let rs = fromMaybe mempty $ M.lookup u (simConnections st)
	in if S.member remotename rs
		then checkKnownRepo (remoteNameToRepoName remotename) st a
		else Left $ "Repository " ++ fromRepoName reponame 
			++ " does not have a remote \"" 
			++ fromRemoteName remotename ++ "\"."

checkValidPreferredContentExpression :: PreferredContentExpression -> v -> Either String v
checkValidPreferredContentExpression expr v =
	case checkPreferredContentExpression expr of
		Nothing -> Right v
		Just e -> Left $ "Failed parsing \"" ++ expr ++ "\": " ++ e

simRandom :: SimState -> (StdGen -> (v, StdGen)) -> (v -> r) -> (r, SimState)
simRandom st mk f =
	let (v, rng) = mk (simRng st)
	in (f v, st { simRng = rng })

randomRepo :: SimState -> (Maybe (RepoName, UUID), SimState)
randomRepo st
	| null (simRepoList st) = (Nothing, st)
	| otherwise = simRandom st
		(randomR (0, length (simRepoList st) - 1)) $ \n -> do
			let r = simRepoList st !! n
			u <- M.lookup r (simRepos st)
			return (r, u)

randomAction :: UUID -> SimState -> (SimAction, SimState)
randomAction u st = case M.lookup u (simConnections st) of
	Just cs | not (S.null cs) ->
		let (mkact, st') = simRandom st (randomR (0, length mkactions - 1))
			(mkactions !!)
		    (remote, st'') = simRandom st' (randomR (0, S.size cs - 1))
		    	(`S.elemAt` cs)
		in (mkact remote, st'')
	-- When there are no remotes, this is the only possible action.
	_ -> (ActionDropUnwanted Nothing, st)
  where
	mkactions =
		[ ActionPull
		, ActionPush
		, ActionGetWanted
		, ActionDropUnwanted . Just
		, const (ActionDropUnwanted Nothing)
		, ActionSendWanted
		, ActionGitPush
		, ActionGitPull
		]

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

newtype GetSimRepoPath = GetSimRepoPath (UUID -> FilePath)

instance Show GetSimRepoPath where
	show _ = "GetSimRepoPath"

data SimRepoConfig = SimRepoConfig
	{ simRepoConfigUUID :: UUID
	, simRepoConfigIsSpecialRemote :: Bool
	, simRepoConfigGroups :: S.Set Group
	, simRepoConfigTrustLevel :: TrustLevel
	, simRepoConfigPreferredContent :: Maybe PreferredContentExpression
	, simRepoConfigRequiredContent :: Maybe PreferredContentExpression
	, simRepoConfigGroupPreferredContent :: M.Map Group PreferredContentExpression
	, simRepoConfigMaxSize :: Maybe MaxSize
	}
	deriving (Show)

newSimRepoConfig :: UUID -> Bool -> SimRepoConfig
newSimRepoConfig u isspecialremote = SimRepoConfig
	{ simRepoConfigUUID = u 
	, simRepoConfigIsSpecialRemote = isspecialremote
	, simRepoConfigGroups = mempty
	, simRepoConfigTrustLevel = def
	, simRepoConfigPreferredContent = Nothing
	, simRepoConfigRequiredContent = Nothing
	, simRepoConfigGroupPreferredContent = mempty
	, simRepoConfigMaxSize = Nothing
	}

addRepo :: RepoName -> SimRepoConfig -> SimState -> SimState
addRepo reponame simrepo st = st
	{ simRepos = M.insert reponame u (simRepos st)
	, simRepoList = if reponame `elem` simRepoList st
		then simRepoList st
		else reponame : simRepoList st
	, simRepoState = M.insert u rst (simRepoState st)
	, simConnections = M.insert u mempty (simConnections st)
	, simGroups = M.insert u (simRepoConfigGroups simrepo) (simGroups st)
	, simTrustLevels = M.insert u
		(simRepoConfigTrustLevel simrepo)
		(simTrustLevels st)
	, simWanted = M.alter
		(const $ simRepoConfigPreferredContent simrepo)
		u
		(simWanted st)
	, simRequired = M.alter
		(const $ simRepoConfigRequiredContent simrepo)
		u
		(simRequired st)
	, simGroupWanted = M.union 
		(simRepoConfigGroupPreferredContent simrepo)
		(simGroupWanted st)
	, simMaxSize = M.alter
		(const $ simRepoConfigMaxSize simrepo)
		u
		(simMaxSize st)
	}
  where
	u = simRepoConfigUUID simrepo
	rst = SimRepoState
		{ simLocations = mempty
		, simIsSpecialRemote = simRepoConfigIsSpecialRemote simrepo
		, simRepo = Nothing
		, simRepoName = reponame
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
					{ simRepoConfigUUID = u
					, simRepoConfigIsSpecialRemote =
						M.member u remoteconfigmap
					, simRepoConfigGroups = gs
					, simRepoConfigTrustLevel =
						lookupTrust' u trustmap
					, simRepoConfigPreferredContent =
						M.lookup u pcmap
					, simRepoConfigRequiredContent =
						M.lookup u rcmap
					, simRepoConfigGroupPreferredContent =
						M.restrictKeys gpcmap gs
					, simRepoConfigMaxSize =
						M.lookup u maxsizes
					}
			(_, msg) -> Left msg

-- Information about a git repository that is cloned and used to represent
-- a repository in the simulation
data SimRepo = SimRepo
	{ simRepoGitRepo :: Repo
	, simRepoAnnex :: (Annex.AnnexState, Annex.AnnexRead)
	, simRepoCurrState :: SimState
	, simRepoUUID :: UUID
	}

instance Show SimRepo where
	show _ = "SimRepo"

{- Inits and updates SimRepos to reflect the SimState. -}
updateSimRepos :: SimState -> IO SimState
updateSimRepos st = updateSimRepoStates st >>= initNewSimRepos

updateSimRepoStates :: SimState -> IO SimState
updateSimRepoStates st = go st (M.toList $ simRepoState st)
  where
	go st' [] = return st'
	go st' ((u, rst):rest) = case simRepo rst of
		Just sr -> do
			sr' <- updateSimRepoState st sr
			let rst' = rst { simRepo = Just sr' }
			let st'' = st
				{ simRepoState = M.insert u rst'
					(simRepoState st)
				}
			go st'' rest
		Nothing -> go st' rest

initNewSimRepos :: SimState -> IO SimState
initNewSimRepos = \st -> go st (M.toList $ simRepoState st)
  where
	go st [] = return st
	go st ((u, rst):rest) =
		case simRepo rst of
			Nothing -> do
				let GetSimRepoPath getdest = simGetSimRepoPath st
				sr <- initSimRepo (simRepoName rst) u (getdest u) st
				let rst' = rst { simRepo = Just sr }
				let st' = st
					{ simRepoState = M.insert u rst'
						(simRepoState st)
					}
				go st' rest
			_ -> go st rest

initSimRepo :: RepoName -> UUID -> FilePath -> SimState -> IO SimRepo
initSimRepo simreponame u dest st = do
	inited <- boolSystem "git" 
		[ Param "init"
		, Param "--quiet"
		, File dest
		]
	unless inited $
		giveup "git init failed"
	simrepo <- Git.Construct.fromPath (toRawFilePath dest)
	ast <- Annex.new simrepo
	((), ast') <- Annex.run ast $ doQuietAction $ do
		storeUUID u
		-- Prevent merging this simulated git-annex branch with
		-- any real one.
		recordDifferences simulationDifferences u
		let desc = simulatedRepositoryDescription simreponame
		initialize startupAnnex (Just desc) Nothing
	updateSimRepoState st $ SimRepo
		{ simRepoGitRepo = simrepo
		, simRepoAnnex = ast'
		, simRepoCurrState = emptySimState
			(simRng st)
			(simGetExistingRepoByName st)
			(simGetSimRepoPath st)
		, simRepoUUID = u
		}

simulatedRepositoryDescription :: RepoName -> String
simulatedRepositoryDescription simreponame = 
	"simulated repository " ++ fromRepoName simreponame

simulationDifferences :: Differences
simulationDifferences = mkDifferences $ S.singleton Simulation

runSimRepo :: UUID -> SimState -> (SimRepoState -> Annex SimState) -> IO SimState
runSimRepo u st a = do
	st' <- updateSimRepos st
	case M.lookup u (simRepoState st') of
		Just rst -> case simRepo rst of
			Just sr -> do
				(st'', strd) <- Annex.run (simRepoAnnex sr) $
					doQuietAction (a rst)
				let sr' = sr
					{ simRepoAnnex = strd
					}
				return $ st''
					{ simRepoState = M.adjust
						(\rst' -> rst' { simRepo = Just sr' })
						u
						(simRepoState st'')
					}
			Nothing -> error $ "runSimRepo simRepo not set for " ++ fromUUID u
		Nothing -> error $ "runSimRepo simRepoState not found for " ++ fromUUID u

updateSimRepoState :: SimState -> SimRepo -> IO SimRepo
updateSimRepoState newst sr = do
	((), (ast, ard)) <- Annex.run (simRepoAnnex sr) $ doQuietAction $ do
		let oldst = simRepoCurrState sr
		updateField oldst newst simRepos $ DiffUpdate
			{ replaceDiff = const . setdesc
			, addDiff = setdesc
			, removeDiff = const $ const noop
			}
		updateField oldst newst simTrustLevels $ DiffUpdate
			{ replaceDiff = const . trustSet
			, addDiff = trustSet
			, removeDiff = const . flip trustSet def
			}
		when (simNumCopies oldst /= simNumCopies newst) $
			setGlobalNumCopies (simNumCopies newst)
		when (simMinCopies oldst /= simMinCopies newst) $
			setGlobalMinCopies (simMinCopies newst)
		updateField oldst newst simGroups $ DiffUpdate
			{ replaceDiff = \u -> const . groupChange u . const
			, addDiff = \u -> groupChange u . const
			, removeDiff = const . flip groupChange (const mempty)
			}
		updateField oldst newst simWanted $ DiffUpdate
			{ replaceDiff = const . preferredContentSet
			, addDiff = preferredContentSet
			, removeDiff = const . flip preferredContentSet mempty
			}
		updateField oldst newst simRequired $ DiffUpdate
			{ replaceDiff = const . requiredContentSet
			, addDiff = requiredContentSet
			, removeDiff = const . flip requiredContentSet mempty
			}
		updateField oldst newst simGroupWanted $ DiffUpdate
			{ replaceDiff = const . groupPreferredContentSet
			, addDiff = groupPreferredContentSet
			, removeDiff = const . flip groupPreferredContentSet mempty
			}
		updateField oldst newst simMaxSize $ DiffUpdate
			{ replaceDiff = const . recordMaxSize
			, addDiff = recordMaxSize
			, removeDiff = const . flip recordMaxSize (MaxSize 0)
			}
		updateField oldst newst getlocations $ DiffUpdate
			{ replaceDiff = \k newls oldls -> do
				let news = getSimLocations' newls
				let olds = getSimLocations' oldls
				setlocations InfoPresent k
					(S.difference news olds)
				setlocations InfoMissing k
					(S.difference olds news)
			, addDiff = \k ls -> setlocations InfoPresent k
				(getSimLocations' ls)
			, removeDiff = \k ls -> setlocations InfoMissing k
				(getSimLocations' ls)
			}
		updateField oldst newst simFiles $ DiffUpdate
			{ replaceDiff = const . stageannexedfile
			, addDiff = stageannexedfile
			, removeDiff = const . unstageannexedfile
			}
		Annex.Queue.flush
	let ard' = ard { Annex.rebalance = simRebalance newst }
	return $ sr
		{ simRepoAnnex = (ast, ard')
		, simRepoCurrState = newst
		}
  where
	setdesc r u = describeUUID u $ toUUIDDesc $
		simulatedRepositoryDescription r
	stageannexedfile f k = do
		let f' = annexedfilepath f
		l <- calcRepo $ gitAnnexLink f' k
		addAnnexLink l f'
	unstageannexedfile f = do
		liftIO $ removeWhenExistsWith R.removeLink $
			annexedfilepath f
	annexedfilepath f = repoPath (simRepoGitRepo sr) P.</> f
	getlocations = maybe mempty simLocations
		. M.lookup (simRepoUUID sr)
		. simRepoState
	setlocations s k =
		mapM_ (\l -> logChange NoLiveUpdate k l s)

data DiffUpdate a b m = DiffUpdate
	{ replaceDiff :: a -> b -> b -> m ()
	-- ^ The first value is the new one, the second is the old one.
	, addDiff :: a -> b -> m ()
	, removeDiff :: a -> b -> m ()
	}

updateMap
	:: (Monad m, Ord a, Eq b)
	=> M.Map a b
	-> M.Map a b
	-> DiffUpdate a b m
	-> m ()
updateMap old new diffupdate = do
	forM_ (M.toList $ M.intersectionWith (,) new old) $ 
		\(k, (newv, oldv))->
			when (newv /= oldv) $
				replaceDiff diffupdate k newv oldv
	forM_ (M.toList $ M.difference new old) $
		uncurry (addDiff diffupdate)
	forM_ (M.toList $ M.difference old new) $
		\(k, oldv) -> removeDiff diffupdate k oldv

updateField
	:: (Monad m, Ord a, Eq b)
	=> v
	-> v
	-> (v -> M.Map a b)
	-> DiffUpdate a b m
	-> m ()
updateField old new f = updateMap (f old) (f new)

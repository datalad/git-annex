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
import Text.Read
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.UUID as U
import qualified Data.UUID.V5 as U5
import qualified Utility.RawFilePath as R
import qualified System.FilePath.ByteString as P

data SimState t = SimState
	{ simRepos :: M.Map RepoName UUID
	, simRepoState :: M.Map UUID (SimRepoState t)
	, simConnections :: M.Map UUID (S.Set RemoteName)
	, simFiles :: M.Map RawFilePath Key
	, simRng :: Int
	, simTrustLevels :: M.Map UUID TrustLevel
	, simNumCopies :: NumCopies
	, simMinCopies :: MinCopies
	, simGroups :: M.Map UUID (S.Set Group)
	, simWanted :: M.Map UUID PreferredContentExpression
	, simRequired :: M.Map UUID PreferredContentExpression
	, simGroupWanted :: M.Map Group PreferredContentExpression
	, simMaxSize :: M.Map UUID MaxSize
	, simRebalance :: Bool
	, simHistory :: [SimCommand]
	, simVectorClock :: VectorClock
	, simFile :: Maybe FilePath
	, simRootDirectory :: FilePath
	}
	deriving (Show, Read)

emptySimState :: Int -> FilePath -> SimState t
emptySimState rngseed rootdir = SimState
	{ simRepos = mempty
	, simRepoState = mempty
	, simConnections = mempty
	, simFiles = mempty
	, simRng = rngseed
	, simTrustLevels = mempty
	, simNumCopies = configuredNumCopies 1
	, simMinCopies = configuredMinCopies 1
	, simGroups = mempty
	, simWanted = mempty
	, simRequired = mempty
	, simGroupWanted = mempty
	, simMaxSize = mempty
	, simRebalance = False
	, simHistory = []
	, simVectorClock = VectorClock 0
	, simFile = Nothing
	, simRootDirectory = rootdir
	}

-- State that can vary between different repos in the simulation.
data SimRepoState t = SimRepoState
	{ simLocations :: M.Map Key (M.Map UUID LocationState)
	, simIsSpecialRemote :: Bool
	, simRepo :: Maybe t
	, simRepoName :: RepoName
	}
	deriving (Show, Read)

data LocationState = LocationState VectorClock Bool
	deriving (Eq, Show, Read)

newtype VectorClock = VectorClock Int
	deriving (Eq, Ord, Show, Read)

newerLocationState ::  LocationState -> LocationState -> LocationState
newerLocationState l1@(LocationState vc1 _) l2@(LocationState vc2 _)
	| vc1 > vc2 = l1
	| otherwise = l2
		
{- Updates the state of stu to indicate that a key is present or not in u. -}
setPresentKey :: Bool -> UUID -> Key -> UUID -> SimState SimRepo -> SimState SimRepo
setPresentKey present u k stu st = st
	{ simRepoState = case M.lookup stu (simRepoState st) of
		Just rst -> M.insert stu
			(setPresentKey' present (simVectorClock st) u k rst)
			(simRepoState st)
		Nothing -> error "no simRepoState in setPresentKey"
	}

setPresentKey' :: Bool -> VectorClock -> UUID -> Key -> SimRepoState t -> SimRepoState t
setPresentKey' present vc u k rst = rst
	{ simLocations = 
		M.insertWith (M.unionWith newerLocationState) k
			(M.singleton u (LocationState vc present))
			(simLocations rst)
	}

getSimLocations :: SimRepoState t -> Key -> S.Set UUID
getSimLocations rst k =
	maybe mempty getSimLocations' $
		M.lookup k (simLocations rst)

getSimLocations' :: M.Map UUID LocationState -> S.Set UUID
getSimLocations' = M.keysSet . M.filter present
  where
	present (LocationState _ b) = b

addHistory :: SimState t -> SimCommand -> SimState t
addHistory st c = st { simHistory = c : simHistory st }

newtype RepoName = RepoName { fromRepoName :: String }
	deriving (Show, Read, Eq, Ord)

newtype RemoteName = RemoteName { fromRemoteName :: String }
	deriving (Show, Read, Eq, Ord)

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
	deriving (Show, Read)

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
	| CommandAddMulti Int String ByteSize ByteSize [RepoName]
	| CommandStep Int
	| CommandAction SimAction
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
	deriving (Show, Read)

data SimAction
	= ActionPull RepoName RemoteName
	| ActionPush RepoName RemoteName
	| ActionSync RepoName RemoteName
	| ActionGetWanted RepoName RemoteName
	| ActionDropUnwanted RepoName (Maybe RemoteName)
	| ActionSendWanted RepoName RemoteName
	| ActionGitPush RepoName RemoteName
	| ActionGitPull RepoName RemoteName
	| ActionWhile SimAction SimAction
	deriving (Show, Read)

runSimCommand :: SimCommand -> GetExistingRepoByName -> SimState SimRepo -> Annex (SimState SimRepo)
runSimCommand (CommandStep n) repobyname st
	| n > 0 = case randomRepo st of
		(Just (repo, u), st') ->
			let (act, st'') = randomAction repo u st'
			in runSimCommand (CommandAction act) repobyname st''
				>>= runSimCommand (CommandStep (pred n)) repobyname
		(Nothing, st') -> return st'
	| otherwise = return st
runSimCommand cmd repobyname st = 
	case applySimCommand cmd st repobyname of
		Left err -> giveup err
		Right (Right st') -> return st'
		Right (Left mkst) -> mkst

applySimCommand
	:: SimCommand
	-> SimState SimRepo
	-> GetExistingRepoByName 
	-> Either String (Either (Annex (SimState SimRepo)) (SimState SimRepo))
applySimCommand cmd st = 
	applySimCommand' cmd $ flip addHistory cmd $ st
		{ simVectorClock = 
			let (VectorClock clk) = simVectorClock st 
			in VectorClock (succ clk)
		}

applySimCommand'
	:: SimCommand
	-> SimState SimRepo
	-> GetExistingRepoByName
	-> Either String (Either (Annex (SimState SimRepo)) (SimState SimRepo))
applySimCommand' (CommandInit reponame) st _ =
	checkNonexistantRepo reponame st $
		let (u, st') = genSimUUID st reponame
		in Right $ Right $ addRepo reponame (newSimRepoConfig u False) st'
applySimCommand' (CommandInitRemote reponame) st _ = 
	checkNonexistantRepo reponame st $
		let (u, st') = genSimUUID st reponame
		in Right $ Right $ addRepo reponame (newSimRepoConfig u True) st'
applySimCommand' (CommandUse reponame s) st repobyname =
	case getExistingRepoByName repobyname s of
		Right existingrepo -> checkNonexistantRepo reponame st $
			Right $ Right $ addRepo reponame existingrepo st
		Left msg -> Left $ "Unable to use a repository \"" 
			++ fromRepoName reponame 
			++ "\" in the simulation because " ++ msg
applySimCommand' (CommandConnect connections) st repobyname =
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
				applySimCommand' (CommandConnect connections') st' repobyname
applySimCommand' (CommandDisconnect connections) st repobyname = 
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
				applySimCommand' (CommandDisconnect connections') st' repobyname
applySimCommand' (CommandAddTree repo expr) st _ =
	checkKnownRepo repo st $ const $
		checkValidPreferredContentExpression expr $ Left $
			error "TODO" -- XXX
applySimCommand' (CommandAdd file sz repos) st _ = 
	let (k, st') = genSimKey sz st
	in go k st' repos
  where
	go _k st' [] = Right $ Right st'
	go k st' (repo:rest) = checkKnownRepo repo st' $ \u ->
		let st'' = setPresentKey True u k u $ st'
			{ simFiles = M.insert file k (simFiles st')
			}
		in go k st'' rest
applySimCommand' (CommandAddMulti n suffix minsz maxsz repos) st repobyname = 
	let (sz, st') = simRandom st (randomR (minsz, maxsz)) id
	    file = toRawFilePath (show n ++ suffix)
	in case applySimCommand' (CommandAdd file sz repos) st' repobyname of
		Left err -> Left err
		Right (Right st'') -> 
			case pred n of
				0 -> Right (Right st'')
				n' -> applySimCommand' (CommandAddMulti n' suffix minsz maxsz repos) st'' repobyname
		Right (Left _) -> error "applySimCommand' CommandAddMulti"
applySimCommand' (CommandStep _) _ _ = error "applySimCommand' CommandStep"
applySimCommand' (CommandAction act) st _ =
	case getSimActionComponents act st of
		Left err -> Left err
		Right (Right st') -> Right (Right st')
		Right (Left (st', l)) -> Right $ Left $ go l st'
  where
	go [] st' = return st'
	go (a:as) st' = a st' >>= go as
applySimCommand' (CommandSeed rngseed) st _ =
	Right $ Right $ st
		{ simRng = rngseed
		}
applySimCommand' (CommandPresent repo file) st _ = checkKnownRepo repo st $ \u ->
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
applySimCommand' (CommandNotPresent repo file) st _ = checkKnownRepo repo st $ \u ->
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
applySimCommand' (CommandNumCopies n) st _ =
	Right $ Right $ st
		{ simNumCopies = configuredNumCopies n
		}
applySimCommand' (CommandMinCopies n) st _ =
	Right $ Right $ st
		{ simMinCopies = configuredMinCopies n
		}
applySimCommand' (CommandTrustLevel repo trustlevel) st _ =
	checkKnownRepo repo st $ \u ->
		 Right $ Right $ st
			{ simTrustLevels = M.insert u trustlevel
				(simTrustLevels st)
			}
applySimCommand' (CommandGroup repo groupname) st _ = 
	checkKnownRepo repo st $ \u ->
		Right $ Right $ st
			{ simGroups = M.insertWith S.union u
				(S.singleton groupname)
				(simGroups st)
			}
applySimCommand' (CommandUngroup repo groupname) st _ = 
	checkKnownRepo repo st $ \u ->
		Right $ Right $ st
			{ simGroups = M.adjust (S.delete groupname) u (simGroups st)
			}
applySimCommand' (CommandWanted repo expr) st _ = 
	checkKnownRepo repo st $ \u ->
		checkValidPreferredContentExpression expr $ Right $ st
			{ simWanted = M.insert u expr (simWanted st)
			}
applySimCommand' (CommandRequired repo expr) st _ = 
	checkKnownRepo repo st $ \u ->
		checkValidPreferredContentExpression expr $ Right $ st
			{ simRequired = M.insert u expr (simRequired st)
			}
applySimCommand' (CommandGroupWanted groupname expr) st _ =
	checkValidPreferredContentExpression expr $ Right $ st
		{ simGroupWanted = M.insert groupname expr (simGroupWanted st)
		}
applySimCommand' (CommandMaxSize repo sz) st _ = 
	checkKnownRepo repo st $ \u ->
		Right $ Right $ st
			{ simMaxSize = M.insert u sz (simMaxSize st)
			}
applySimCommand' (CommandRebalance b) st _ = 
	Right $ Right $ st
		{ simRebalance = b
		}
applySimCommand' (CommandComment _) st _ = Right $ Right st
applySimCommand' CommandBlank st _ = Right $ Right st

getSimActionComponents
	:: SimAction
	-> SimState SimRepo
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo)]) (SimState SimRepo))
getSimActionComponents (ActionGetWanted repo remote) st =
	checkKnownRepo repo st $ \u -> 
		let go _remoteu f k _r st' = setPresentKey True u k u $
			addHistory st' $ CommandPresent repo f
		in overFilesRemote repo u remote S.member wanted go st
  where
	wanted k f _ = wantGet NoLiveUpdate False k f
getSimActionComponents (ActionSendWanted repo remote) st = 
	checkKnownRepo repo st $ \u ->
		overFilesRemote repo u remote S.notMember wanted (go u) st
  where
	wanted = wantGetBy NoLiveUpdate False
	go u remoteu f k _r st' = 
		-- Sending to a remote updates the location log
		-- of both the repository sending and the remote.
		setPresentKey True remoteu k remoteu $
		setPresentKey True remoteu k u $
		addHistory st' $ CommandPresent (remoteNameToRepoName remote) f
getSimActionComponents (ActionDropUnwanted repo Nothing) st =
	checkKnownRepo repo st $ \u ->
		Right $ Left (st, map (go u) $ M.toList $ simFiles st)
  where
	go u (f, k) st' = liftIO $ runSimRepo u st' $ \rst ->
		let af = AssociatedFile $ Just f
		in if present u rst k
			then ifM (wantDrop NoLiveUpdate False Nothing (Just k) af Nothing)
				( return $ checkdrop u rst k st'
				, return st'
				)
			else return st'
	
	present u rst k = u `S.member` getSimLocations rst k

	checkdrop u rst k st' =
		let numcopies = simNumCopies st'
		    mincopies = simMinCopies st'
		    verifiedcopies = mapMaybe (verifypresent u k st') $
		    	filter (/= u) $ S.toList $ getSimLocations rst k
		in case safeDropAnalysis numcopies mincopies verifiedcopies Nothing of
			UnsafeDrop -> st'
			SafeDrop -> dodrop u k st'
			SafeDropCheckTime -> dodrop u k st'

	dodrop u k = setPresentKey False u k u
	
	remotes u = S.fromList $ mapMaybe 
		(\remote -> M.lookup (remoteNameToRepoName remote) (simRepos st))
		(maybe mempty S.toList $ M.lookup u $ simConnections st)
	
	verifypresent u k st' ru = do
		rst <- M.lookup ru (simRepoState st')
		if present ru rst k
			then if ru `S.member` remotes u
				then Just $ if simIsSpecialRemote rst
					then mkVerifiedCopy RecentlyVerifiedCopy ru
					else mkVerifiedCopy LockedCopy ru
				else case M.lookup ru (simTrustLevels st') of
					Just Trusted -> Just $
						mkVerifiedCopy TrustedCopy ru
					_ -> Nothing
			else Nothing
getSimActionComponents (ActionDropUnwanted _repo (Just _remote)) _st =
	undefined -- TODO
getSimActionComponents (ActionGitPush repo remote) st =
	checkKnownRepo repo st $ \u -> 
		checkKnownRemote remote repo u st $ \_ ->
			simulateGitAnnexMerge repo (remoteNameToRepoName remote) st
getSimActionComponents (ActionGitPull repo remote) st =
	checkKnownRepo repo st $ \u -> 
		checkKnownRemote remote repo u st $ \_ ->
			simulateGitAnnexMerge (remoteNameToRepoName remote) repo st
getSimActionComponents (ActionWhile a b) st =
	case getSimActionComponents a st of
		Left err -> Left err
		Right (Right st') -> getSimActionComponents b st'
		Right (Left (st', as)) ->
			case getSimActionComponents b st' of
				Left err -> Left err
				Right (Right st'') -> Right $ Left (st'', as)
				Right (Left (st'', bs)) ->
					Right $ Left $ mingle as bs st'' []
  where
	mingle [] subbs st' c = (st', reverse c ++ subbs)
	mingle subas [] st' c = (st', reverse c ++ subas)
	mingle (suba:subas) (subb:subbs) st' c = 
		let (coinflip, st'') = simRandom st' random id
		in if coinflip
			then mingle subas (subb:subbs) st'' (suba:c)
			else mingle (suba:subas) subbs st'' (subb:c)
getSimActionComponents (ActionPull repo remote) st =
	simActionSequence
		[ ActionGitPull repo remote
		, ActionGetWanted repo remote
		, ActionDropUnwanted repo Nothing
		] st
getSimActionComponents (ActionPush repo remote) st =
	simActionSequence
		[ ActionSendWanted repo remote
		, ActionDropUnwanted repo (Just remote)
		, ActionGitPush repo remote
		] st
getSimActionComponents (ActionSync repo remote) st =
	simActionSequence
		[ ActionGitPull repo remote
		, ActionGetWanted repo remote
		, ActionSendWanted repo remote
		, ActionDropUnwanted repo (Just remote)
		, ActionGitPush repo remote
		] st

simActionSequence
	:: [SimAction]
	-> SimState SimRepo
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo)]) (SimState SimRepo))
simActionSequence [] st = Right (Right st)
simActionSequence (a:as) st = case getSimActionComponents a st of
	Left err -> Left err
	Right (Right st') -> simActionSequence as st'
	Right (Left (st', subas)) -> go st' subas as
  where
	go st' c [] = Right $ Left (st', c)
	go st' c (a':as') = case getSimActionComponents a' st' of
		Left err -> Left err
		Right (Right st'') -> go st'' c as'
		Right (Left (st'', subas)) -> go st'' (c ++ subas) as'

overFilesRemote
	:: RepoName
	-> UUID
	-> RemoteName
	-> (UUID -> S.Set UUID -> Bool) 
	-> (Maybe Key -> AssociatedFile -> UUID -> Annex Bool)
        -> (UUID -> RawFilePath -> Key -> RepoName -> SimState SimRepo -> SimState SimRepo)
	-> SimState SimRepo
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo)]) (SimState SimRepo))
overFilesRemote r u remote remotepred checkwant handlewanted st = 
	checkKnownRemote remote r u st $ \remoteu ->
		Right (Left (st, map (go remoteu) $ M.toList $ simFiles st))
  where
	go remoteu (f, k) st' = 
	  	let af = AssociatedFile $ Just f
		in liftIO $ runSimRepo u st' $ \rst ->
			if checkremotepred remoteu rst k
				then ifM (checkwant (Just k) af remoteu)
					( return $ handlewanted remoteu f k r st'
					, return st'
					)
				else return st'
	checkremotepred remoteu rst k =
		remotepred remoteu (getSimLocations rst k)

simulateGitAnnexMerge
	:: RepoName
	-> RepoName
	-> SimState SimRepo
	-> Either String (Either a (SimState SimRepo))
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

checkNonexistantRepo :: RepoName -> SimState SimRepo -> Either String a -> Either String a
checkNonexistantRepo reponame st a = case M.lookup reponame (simRepos st) of
	Nothing -> a
	Just _ -> Left $ "There is already a repository in the simulation named \""
		++ fromRepoName reponame ++ "\"."

checkKnownRepo :: RepoName -> SimState SimRepo -> (UUID -> Either String a) -> Either String a
checkKnownRepo reponame st a = case M.lookup reponame (simRepos st) of
	Just u -> a u
	Nothing -> Left $ "No repository in the simulation is named \""
		++ fromRepoName reponame ++ "\"."

checkKnownRemote :: RemoteName -> RepoName -> UUID -> SimState SimRepo -> (UUID -> Either String a) -> Either String a
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

simRandom :: SimState t -> (StdGen -> (v, StdGen)) -> (v -> r) -> (r, SimState t)
simRandom st mk f =
	let rng = mkStdGen (simRng st)
	    (v, rng') = mk rng
	    (newseed, _) = random rng'
	in (f v, st { simRng = newseed })

randomRepo :: SimState SimRepo -> (Maybe (RepoName, UUID), SimState SimRepo)
randomRepo st
	| null repolist = (Nothing, st)
	| otherwise = simRandom st
		(randomR (0, length repolist - 1)) $ \n -> do
			let r = repolist !! n
			u <- M.lookup r (simRepos st)
			return (r, u)
  where
	repolist = M.keys (simRepos st)

randomAction :: RepoName -> UUID -> SimState SimRepo -> (SimAction, SimState SimRepo)
randomAction repo u st = case M.lookup u (simConnections st) of
	Just cs | not (S.null cs) ->
		let (mkact, st') = simRandom st (randomR (0, length mkactions - 1))
			(mkactions !!)
		    (remote, st'') = simRandom st' (randomR (0, S.size cs - 1))
		    	(`S.elemAt` cs)
		in (mkact repo remote, st'')
	-- When there are no remotes, this is the only possible action.
	_ -> (ActionDropUnwanted repo Nothing, st)
  where
	mkactions =
		[ ActionPull
		, ActionPush
		, ActionSync
		, ActionGetWanted
		, \repo' remote -> ActionDropUnwanted repo' (Just remote)
		, \repo' _remote -> ActionDropUnwanted repo' Nothing
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

genSimKey :: ByteSize -> SimState t -> (Key, SimState t)
genSimKey sz st = simRandom st (randomWords 1024) mk
  where
	mk b =
		let tk = genTestKey $ L.pack b
		in alterKey tk $ \kd -> kd { keySize = Just sz }

genSimUUID :: SimState t -> RepoName -> (UUID, SimState t)
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

addRepo :: RepoName -> SimRepoConfig -> SimState SimRepo -> SimState SimRepo
addRepo reponame simrepo st = st
	{ simRepos = M.insert reponame u (simRepos st)
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
	, simRepoCurrState :: SimState SimRepo
	, simRepoUUID :: UUID
	}

instance Show SimRepo where
	show _ = "SimRepo"

{- Inits and updates SimRepos to reflect the SimState. -}
updateSimRepos :: SimState SimRepo -> IO (SimState SimRepo)
updateSimRepos st = updateSimRepoStates st >>= initNewSimRepos

updateSimRepoStates :: SimState SimRepo -> IO (SimState SimRepo)
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

initNewSimRepos :: SimState SimRepo -> IO (SimState SimRepo)
initNewSimRepos = \st -> go st (M.toList $ simRepoState st)
  where
	go st [] = return st
	go st ((u, rst):rest) =
		case simRepo rst of
			Nothing -> do
				let d = simRepoDirectory st u
				sr <- initSimRepo (simRepoName rst) u d st
				let rst' = rst { simRepo = Just sr }
				let st' = st
					{ simRepoState = M.insert u rst'
						(simRepoState st)
					}
				go st' rest
			_ -> go st rest

simRepoDirectory :: SimState t -> UUID -> FilePath
simRepoDirectory st u = simRootDirectory st </> fromUUID u

initSimRepo :: RepoName -> UUID -> FilePath -> SimState SimRepo -> IO SimRepo
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
		, simRepoCurrState = 
			emptySimState (simRng st) (simRootDirectory st)
		, simRepoUUID = u
		}

simulatedRepositoryDescription :: RepoName -> String
simulatedRepositoryDescription simreponame = 
	"simulated repository " ++ fromRepoName simreponame

simulationDifferences :: Differences
simulationDifferences = mkDifferences $ S.singleton Simulation

runSimRepo :: UUID -> SimState SimRepo -> (SimRepoState SimRepo -> Annex (SimState SimRepo)) -> IO (SimState SimRepo)
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

updateSimRepoState :: SimState SimRepo -> SimRepo -> IO SimRepo
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
			{ replaceDiff = replaceNew preferredContentSet
			, addDiff = preferredContentSet
			, removeDiff = const . flip preferredContentSet mempty
			}
		updateField oldst newst simRequired $ DiffUpdate
			{ replaceDiff = replaceNew requiredContentSet
			, addDiff = requiredContentSet
			, removeDiff = const . flip requiredContentSet mempty
			}
		updateField oldst newst simGroupWanted $ DiffUpdate
			{ replaceDiff = replaceNew groupPreferredContentSet
			, addDiff = groupPreferredContentSet
			, removeDiff = const . flip groupPreferredContentSet mempty
			}
		updateField oldst newst simMaxSize $ DiffUpdate
			{ replaceDiff = replaceNew recordMaxSize
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
			{ replaceDiff = replaceNew stageannexedfile
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

replaceNew :: (a -> b -> m ()) -> a -> b -> b -> m ()
replaceNew f a new _old = f a new

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

suspendSim :: SimState SimRepo -> IO ()
suspendSim st = do
	-- Update the sim repos before suspending, so that at restore time
	-- they are up-to-date.
	st' <- updateSimRepos st
	let st'' = st'
		{ simRepoState = M.map freeze (simRepoState st)
		}
	writeFile (simRootDirectory st </> "state") (show st'')
  where
	freeze :: SimRepoState SimRepo -> SimRepoState ()
	freeze rst = rst { simRepo = Nothing }

restoreSim :: RawFilePath -> IO (Either String (SimState SimRepo))
restoreSim rootdir = 
	tryIO (readFile (fromRawFilePath rootdir </> "state")) >>= \case
		Left err -> return (Left (show err))
		Right c -> case readMaybe c :: Maybe (SimState ()) of
			Nothing -> return (Left "unable to parse sim state file")
			Just st -> do
				repostate <- M.fromList
					<$> mapM (thaw st) (M.toList (simRepoState st))
				let st' = st
					{ simRepoState = 
						M.map (finishthaw st') repostate
					}
				return (Right st')
  where
	thaw st (u, rst) = tryNonAsync (thaw' st u) >>= return . \case
		Left _ -> (u, rst { simRepo = Nothing })
		Right r -> (u, rst { simRepo = Just r })
	thaw' st u = do
		simrepo <- Git.Construct.fromPath $ toRawFilePath $
			simRepoDirectory st u
		ast <- Annex.new simrepo
		return $ SimRepo
			{ simRepoGitRepo = simrepo
			, simRepoAnnex = ast
			, simRepoCurrState =
				-- Placeholder, replaced later with current
				-- state.
				emptySimState (simRng st)
					(simRootDirectory st)
			, simRepoUUID = u
			}
	finishthaw st rst = rst
		{ simRepo = case simRepo rst of
			Nothing -> Nothing
			Just sr -> Just $ sr { simRepoCurrState = st }
		}

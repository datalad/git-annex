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
import Git.FilePath
import Backend.Hash (genTestKey)
import Annex.UUID
import Annex.FileMatcher
import Annex.Init
import Annex.Startup
import Annex.Link
import Annex.Wanted
import Annex.CatFile
import Annex.Action (quiesce)
import Logs.Group
import Logs.Trust
import Logs.PreferredContent
import Logs.NumCopies
import Logs.Remote
import Logs.MaxSize
import Logs.Difference
import Logs.UUID
import Logs.Location
import Utility.Env
import qualified Annex
import qualified Remote
import qualified Git.Construct
import qualified Git.LsFiles
import qualified Annex.Queue
import qualified Database.RepoSize

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
	, simClusterNodes :: M.Map RepoName UUID
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
	, simRootDirectory :: FilePath
	, simFailed :: Bool
	}
	deriving (Show, Read)

emptySimState :: Int -> FilePath -> SimState t
emptySimState rngseed rootdir = SimState
	{ simRepos = mempty
	, simRepoState = mempty
	, simConnections = mempty
	, simClusterNodes = mempty
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
	, simRootDirectory = rootdir
	, simFailed = False
	}

-- State that can vary between different repos in the simulation.
data SimRepoState t = SimRepoState
	{ simLocations :: M.Map Key (M.Map UUID LocationState)
	, simLiveSizeChanges :: M.Map UUID SizeOffset
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
		
{- Updates the state of stu to indicate that a key is present or not in u.
 -
 - Also, when the reponame is the name of a cluster node, updates
 - the state of every other repository that has a connection to that
 - same cluster node.
 -}
setPresentKey :: Bool -> (UUID, RepoName) -> Key -> UUID -> SimState SimRepo -> SimState SimRepo
setPresentKey present (u, reponame) k stu st = handleclusters $ st
	{ simRepoState = case M.lookup stu (simRepoState st) of
		Just rst -> M.insert stu
			(setPresentKey' present (simVectorClock st) u k rst)
			(simRepoState st)
		Nothing -> error "no simRepoState in setPresentKey"
	}
  where
	handleclusters st' = case M.lookup reponame (simClusterNodes st') of
		Just u' | u' == u -> handleclusters' st' $ 
			filter (/= stu) $ M.keys $ 
				M.filter (S.member (repoNameToRemoteName reponame))
					(simConnections st')
		_ -> st'
	handleclusters' st' [] = st'
	handleclusters' st' (cu:cus) =
		flip handleclusters' cus $ st'
			{ simRepoState = case M.lookup cu (simRepoState st') of
				Just rst -> M.insert cu
					(setPresentKey' present (simVectorClock st') u k rst)
					(simRepoState st')
				Nothing -> simRepoState st'
			}

setPresentKey' :: Bool -> VectorClock -> UUID -> Key -> SimRepoState t -> SimRepoState t
setPresentKey' present vc u k rst = rememberLiveSizeChanges present u k rst $ rst
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

recordSeed :: SimState t -> [SimCommand] -> SimState t
recordSeed st (CommandSeed _:_) = st
recordSeed st _ = addHistory st (CommandSeed (simRng st))

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
	| CommandStepStable Int
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
	| CommandRandomWanted RepoName [PreferredContentExpression]
	| CommandRandomRequired RepoName [PreferredContentExpression]
	| CommandRandomGroupWanted Group [PreferredContentExpression]
	| CommandMaxSize RepoName MaxSize
	| CommandRebalance Bool
	| CommandClusterNode RepoName RepoName
	| CommandVisit RepoName [String]
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
applySimCommand (CommandPresent repo file) st _ = checkKnownRepo repo st $ \u ->
	case (M.lookup file (simFiles st), M.lookup u (simRepoState st)) of
		(Just k, Just rst)
			| u `S.member` getSimLocations rst k ->
				Right $ Right st
			| otherwise -> missing
		(Just _, Nothing) -> missing
		(Nothing, _) -> Right $ Left $ do
			showLongNote $ UnquotedString $
				"Expected " ++ fromRawFilePath file
					++ " to be present in " ++ fromRepoName repo 
					++ ", but the simulation does not include that file."
			return $ st { simFailed = True }
  where
	missing = Right $ Left $ do
		showLongNote $ UnquotedString $
			"Expected " ++ fromRawFilePath file
				++ " to be present in " 
				++ fromRepoName repo ++ ", but it is not."
		return $ st { simFailed = True }
applySimCommand (CommandNotPresent repo file) st _ = checkKnownRepo repo st $ \u ->
	case (M.lookup file (simFiles st), M.lookup u (simRepoState st)) of
		(Just k, Just rst)
			| u `S.notMember` getSimLocations rst k ->
				Right $ Right st
			| otherwise -> present
		(Just _, Nothing) -> present
		(Nothing, _) -> Right $ Left $ do
			showLongNote $ UnquotedString $ 
				"Expected " ++ fromRawFilePath file
					++ " to not be present in " ++ fromRepoName repo 
					++ ", but the simulation does not include that file."
			return $ st { simFailed = True }
  where
	present = Right $ Left $ do
		showLongNote $ UnquotedString $
			"Expected " ++ fromRawFilePath file 
			++ " not to be present in " 
			++ fromRepoName repo ++ ", but it is present."
		return $ st { simFailed = True }
applySimCommand c@(CommandVisit repo cmdparams) st _ =
	checkKnownRepo repo st $ \u -> Right $ Left $ do
		st' <- liftIO $ updateSimRepos st
		let dir = simRepoDirectory st' u
		unlessM (liftIO $ doesDirectoryExist dir) $
			giveup "Simulated repository unavailable."
		(cmd, params) <- case cmdparams of
			[] -> do
				showLongNote "Starting a shell in the simulated repository."
				shellcmd <- liftIO $ fromMaybe "sh" <$> getEnv "SHELL"
				return (shellcmd, [])
			_ -> return ("sh", ["-c", unwords cmdparams])
		exitcode <- liftIO $
			safeSystem' cmd (map Param params)
				(\p -> p { cwd = Just dir })
		when (null cmdparams) $
			showLongNote "Finished visit to simulated repository."
		if null cmdparams
			then return st'
			else if exitcode == ExitSuccess
				then return $ addHistory st' c
				else do
					showLongNote $ UnquotedString $ 
						"Command " ++ unwords cmdparams ++
							" exited nonzero."
					liftIO $ exitWith exitcode
applySimCommand cmd st repobyname = 
	let st' = flip addHistory cmd $ st
		{ simVectorClock = 
			let (VectorClock clk) = simVectorClock st 
			in VectorClock (succ clk)
		}
	in applySimCommand' cmd st' repobyname

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
		if maybe False simIsSpecialRemote (M.lookup u (simRepoState st))
			then Left $ fromRepoName repo ++ " is a special remote, and cannot connect to " ++ fromRemoteName remote
			else go u remote mconnections
  where
	go u remote mconnections =
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
	checkKnownRepo repo st $ \u ->
		checkValidPreferredContentExpression [expr] $ Left $ do
			matcher <- makematcher u
			(l, cleanup) <- inRepo $ Git.LsFiles.inRepo [] []
			st' <- go matcher u st l
			liftIO $ void cleanup
			return st'
  where
	go _ _ st' [] = return st'
	go matcher u st' (f:fs) = catKeyFile f  >>= \case
		Just k -> do
			afile <- AssociatedFile . Just . getTopFilePath
				<$> inRepo (toTopFilePath f)
			ifM (checkMatcher matcher (Just k) afile NoLiveUpdate mempty (pure False) (pure False))
				( let st'' = setPresentKey True (u, repo) k u $ st'
					{ simFiles = M.insert f k (simFiles st')
					}
				  in go matcher u st'' fs
				, go matcher u st' fs 
				)
		Nothing -> go matcher u st' fs
	makematcher :: UUID -> Annex (FileMatcher Annex)
	makematcher u = do
		groupmap <- groupMap
		configmap <- remoteConfigMap
		gm <- groupPreferredContentMapRaw
		case makeMatcher groupmap configmap gm u id preferredContentTokens parseerr expr of
			Right matcher -> return
				( matcher
				, MatcherDesc "provided preferred content expression"
				)
			Left err -> giveup err
	parseerr = Left "preferred content expression parse error"
applySimCommand' (CommandAdd file sz repos) st _ = 
	let (k, st') = genSimKey sz st
	in go k st' repos
  where
	go _k st' [] = Right $ Right st'
	go k st' (repo:rest) = checkKnownRepo repo st' $ \u ->
		let st'' = setPresentKey True (u, repo) k u $ st'
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
applySimCommand' (CommandStep n) st _ = 
	Right $ Left $ handleStep False n n st
applySimCommand' (CommandStepStable n) st _ = 
	Right $ Left $ handleStep True n n st
applySimCommand' (CommandAction act) st _ =
	case getSimActionComponents act st of
		Left err -> Left err
		Right (Right st') -> Right (Right st')
		Right (Left (st', l)) -> Right $ Left $ go l st'
  where
	go [] st' = return st'
	go (a:as) st' = do
		(st'', _) <- a st'
		go as st''
applySimCommand' (CommandSeed rngseed) st _ =
	Right $ Right $ st
		{ simRng = rngseed
		}
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
		checkValidPreferredContentExpression [expr] $ Right $ st
			{ simWanted = M.insert u expr (simWanted st)
			}
applySimCommand' (CommandRequired repo expr) st _ = 
	checkKnownRepo repo st $ \u ->
		checkValidPreferredContentExpression [expr] $ Right $ st
			{ simRequired = M.insert u expr (simRequired st)
			}
applySimCommand' (CommandGroupWanted groupname expr) st _ =
	checkValidPreferredContentExpression [expr] $ Right $ st
		{ simGroupWanted = M.insert groupname expr (simGroupWanted st)
		}
applySimCommand' (CommandRandomWanted repo terms) st _ = 
	checkKnownRepo repo st $ \u ->
		checkValidPreferredContentExpression terms $ Right $
			randomPreferredContentExpression st terms $ \(expr, st') -> st' 
				{ simWanted = M.insert u expr (simWanted st')
				}
applySimCommand' (CommandRandomRequired repo terms) st _ = 
	checkKnownRepo repo st $ \u ->
		checkValidPreferredContentExpression terms $ Right $
			randomPreferredContentExpression st terms $ \(expr, st') -> st' 
				{ simRequired = M.insert u expr (simRequired st)
				}
applySimCommand' (CommandRandomGroupWanted groupname terms) st _ =
	checkValidPreferredContentExpression terms $ Right $
		randomPreferredContentExpression st terms $ \(expr, st') -> st' 
			{ simGroupWanted = M.insert groupname expr (simGroupWanted st)
			}
applySimCommand' (CommandMaxSize repo sz) st _ = 
	checkKnownRepo repo st $ \u ->
		Right $ Right $ st
			{ simMaxSize = M.insert u sz (simMaxSize st)
			}
applySimCommand' (CommandClusterNode nodename repo) st _ =
	checkKnownRepo repo st $ \u ->
		checkNonexistantRepo nodename st $
			Right $ Right $ st
				{ simClusterNodes = M.insert nodename u
					(simClusterNodes st)
				}
applySimCommand' (CommandRebalance b) st _ = 
	Right $ Right $ st
		{ simRebalance = b
		}
applySimCommand' (CommandComment _) st _ = Right $ Right st
applySimCommand' CommandBlank st _ = Right $ Right st
applySimCommand' (CommandVisit _ _) _ _ = error "applySimCommand' CommandVisit"
applySimCommand' (CommandPresent _ _) _ _ = error "applySimCommand' CommandPresent"
applySimCommand' (CommandNotPresent _ _) _ _ = error "applySimCommand' CommandNotPresent"

handleStep :: Bool -> Int -> Int -> SimState SimRepo -> Annex (SimState SimRepo)
handleStep muststabilize startn n st
	| n >= 0 = do
		let (st', actions) = getactions unsyncactions st
		(st'', restactions) <- runoneaction actions st'
		if null restactions
			then do
				let (st''', actions') = getactions [ActionSync] st''
				(st'''', restactions') <- runoneaction actions' st'''
				if null restactions'
					then do
						showLongNote $ UnquotedString $ 
							"Simulation has stabilized after "
							++ show (startn - n)
							++ " steps."
						return st''''
					else runrest restactions' st'''' (pred n)
			else runrest restactions st'' (pred n)
	| otherwise = checkstabalized st
  where
	runrest actions st' n'
		| n' >= 0 = do
			(st'', restactions) <- runoneaction actions st'
			if null restactions
				then handleStep muststabilize startn n' st'
				else runrest restactions st'' (pred n')
		| otherwise = checkstabalized st'

	checkstabalized st'
		| muststabilize = do
			showLongNote $ UnquotedString $ 
				"Simulation failed to stabilize after "
					++ show startn ++ " steps."
			return $ st' { simFailed = True }
		| otherwise = return st'

	unsyncactions = 
		[ ActionGetWanted
		, ActionSendWanted
		, \repo remote -> ActionDropUnwanted repo (Just remote)
		]

	getactions mks st' = getcomponents [] st' $
		getactions' mks [] (M.toList (simRepos st'))

	getactions' _ c [] = concat c
	getactions' mks c ((repo, u):repos) = 
		case M.lookup u (simConnections st) of
			Nothing -> getactions' mks c repos
			Just remotes ->
				let l = [mk repo remote | remote <- S.toList remotes, mk <- mks]
				in getactions' mks (l:c) repos
	
	getcomponents c st' [] = (st', concat c)
	getcomponents c st' (a:as) = case getSimActionComponents a st' of
		Left _ -> getcomponents c st' as
		Right (Right st'') -> getcomponents c st'' as
		Right (Left (st'', cs)) -> getcomponents (cs:c) st'' as
	
	runoneaction [] st' = return (st', [])
	runoneaction actions st' = do
		let (idx, st'') = simRandom st'
                  	(randomR (0, length actions - 1))
			id
		let action = actions !! idx
		let restactions = take idx actions ++ drop (idx+1) actions
		action st'' >>= \case
			(st''', False) -> runoneaction restactions st'''
			(st''', True) -> return (st''', restactions)

getSimActionComponents
	:: SimAction
	-> SimState SimRepo
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo, Bool)]) (SimState SimRepo))
getSimActionComponents (ActionGetWanted repo remote) st =
	checkKnownRepoNotSpecialRemote repo st $ \u -> 
		let go _remoteu f k _r st' = setPresentKey True (u, repo) k u $
			addHistory st' $ CommandPresent repo f
		in overFilesRemote repo u remote S.member S.notMember wanted go st
  where
	wanted k f _ = wantGet NoLiveUpdate False k f
getSimActionComponents (ActionSendWanted repo remote) st = 
	checkKnownRepoNotSpecialRemote repo st $ \u ->
		overFilesRemote repo u remote S.notMember S.member wanted (go u) st
  where
	wanted = wantGetBy NoLiveUpdate False
	go u remoteu f k _r st' = 
		-- Sending to a remote updates the location log
		-- of both the repository sending and the remote.
		setpresent remoteu $
		setpresent u $
		addHistory st' $ CommandPresent (remoteNameToRepoName remote) f
	  where
	  	setpresent = setPresentKey True (remoteu, remoteNameToRepoName remote) k
getSimActionComponents (ActionDropUnwanted repo Nothing) st =
	checkKnownRepoNotSpecialRemote repo st $ \u ->
		simulateDropUnwanted st u repo u
getSimActionComponents (ActionDropUnwanted repo (Just remote)) st =
	checkKnownRepo repo st $ \u ->
		checkKnownRemote remote repo u st $ \ru ->
			simulateDropUnwanted st u (remoteNameToRepoName remote) ru
getSimActionComponents (ActionGitPush repo remote) st =
	checkKnownRepoNotSpecialRemote repo st $ \u -> 
		checkKnownRemote remote repo u st $ \_ ->
			simulateGitAnnexMerge repo (remoteNameToRepoName remote) st
getSimActionComponents (ActionGitPull repo remote) st =
	checkKnownRepoNotSpecialRemote repo st $ \u -> 
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
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo, Bool)]) (SimState SimRepo))
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
	-> (UUID -> S.Set UUID -> Bool) 
	-> (Maybe Key -> AssociatedFile -> UUID -> Annex Bool)
        -> (UUID -> RawFilePath -> Key -> RepoName -> SimState SimRepo -> SimState SimRepo)
	-> SimState SimRepo
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo, Bool)]) (SimState SimRepo))
overFilesRemote r u remote remotepred localpred checkwant handlewanted st = 
	checkKnownRemote remote r u st $ \remoteu ->
		Right (Left (st, map (go remoteu) $ M.toList $ simFiles st))
  where
	go remoteu (f, k) st' = 
	  	let af = AssociatedFile $ Just f
		in liftIO $ runSimRepo u st' $ \st'' rst ->
			case M.lookup remoteu (simRepoState st'') of
				Nothing -> return (st'', False)
				Just rmtst
					| not (checkremotepred remoteu rst k) -> return (st'', False)
					| not (checkremotepred remoteu rmtst k) -> return (st'', False)
					| not (checklocalpred rst k) -> return (st'', False)
					| otherwise -> updateLiveSizeChanges rst $
						ifM (checkwant (Just k) af remoteu)
							( return (handlewanted remoteu f k r st'', True)
							, return (st'', False)
							)
	checkremotepred remoteu rst k =
		remotepred remoteu (getSimLocations rst k)
	checklocalpred rst k =
		localpred u (getSimLocations rst k)

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
					simulateGitAnnexMerge' srcst destst destu st
		_ -> Left $ "Unable to find " ++ fromRepoName src ++ " or " ++ fromRepoName dest ++ " in simRepos"

simulateGitAnnexMerge' :: SimRepoState SimRepo -> SimRepoState SimRepo -> UUID -> SimState SimRepo -> SimState SimRepo
simulateGitAnnexMerge' srcst destst destu st = 
	let locs = M.unionWith
		(M.unionWith newerLocationState)
		(simLocations destst)
		(simLocations srcst)
	    destst' = calcLiveSizeChanges $ destst
	    	{ simLocations = locs
		}
	in st
		{ simRepoState = M.insert destu destst' (simRepoState st)
		}

simulateDropUnwanted
	:: SimState SimRepo
	-> UUID
	-> RepoName
	-> UUID
	-> Either String (Either (SimState SimRepo, [SimState SimRepo -> Annex (SimState SimRepo, Bool)]) (SimState SimRepo))
simulateDropUnwanted st u dropfromname dropfrom =
	Right $ Left (st, map go $ M.toList $ simFiles st)
  where
	go (f, k) st' = liftIO $ runSimRepo u st' $ \st'' rst ->
		let af = AssociatedFile $ Just f
		in if present dropfrom rst k
			then updateLiveSizeChanges rst $
				ifM (wantDrop NoLiveUpdate False (Just dropfrom) (Just k) af Nothing)
					( return $ checkdrop rst k f st''
					, return (st'', False)
					)
			else return (st'', False)

	present ru rst k = ru `S.member` getSimLocations rst k

	checkdrop rst k f st' =
		let numcopies = simNumCopies st'
		    mincopies = simMinCopies st'
		    verifiedcopies = mapMaybe (verifypresent k st') $
		    	filter (/= dropfrom) $ S.toList $ getSimLocations rst k
		in case safeDropAnalysis numcopies mincopies verifiedcopies Nothing of
			UnsafeDrop -> (st', False)
			SafeDrop -> (dodrop k f st', True)
			SafeDropCheckTime -> (dodrop k f st', True)

	dodrop k f st' =
		setPresentKey False (dropfrom, dropfromname) k u $
			setPresentKey False (dropfrom, dropfromname) k dropfrom $
				addHistory st' $ CommandNotPresent dropfromname f
	
	remotes = S.fromList $ mapMaybe 
		(\remote -> M.lookup (remoteNameToRepoName remote) (simRepos st))
		(maybe mempty S.toList $ M.lookup u $ simConnections st)
	
	verifypresent k st' ru = do
		rst <- M.lookup ru (simRepoState st')
		if present ru rst k
			then if ru `S.member` remotes || ru == u
				then Just $ if simIsSpecialRemote rst
					then mkVerifiedCopy RecentlyVerifiedCopy ru
					else mkVerifiedCopy LockedCopy ru
				else case M.lookup ru (simTrustLevels st') of
					Just Trusted -> Just $
						mkVerifiedCopy TrustedCopy ru
					_ -> Nothing
			else Nothing

checkNonexistantRepo :: RepoName -> SimState SimRepo -> Either String a -> Either String a
checkNonexistantRepo reponame st a = case M.lookup reponame (simRepos st) of
	Nothing -> case M.lookup reponame (simClusterNodes st) of
		Just _ -> Left $ "There is already a cluster node in the simulation named \""
			++ fromRepoName reponame ++ "\"."
		Nothing -> a
	Just _ -> Left $ "There is already a repository in the simulation named \""
		++ fromRepoName reponame ++ "\"."

checkKnownRepo :: RepoName -> SimState SimRepo -> (UUID -> Either String a) -> Either String a
checkKnownRepo reponame st a = case M.lookup reponame (simRepos st) of
	Just u -> a u
	Nothing -> case M.lookup reponame (simClusterNodes st) of
		Just u -> a u
		Nothing -> Left $ "No repository in the simulation is named \""
			++ fromRepoName reponame ++ "\". Choose from: "
			++ unwords (map fromRepoName $ M.keys (simRepos st))

checkKnownRepoNotSpecialRemote :: RepoName -> SimState SimRepo -> (UUID -> Either String a) -> Either String a
checkKnownRepoNotSpecialRemote reponame st a =
	checkKnownRepo reponame st $ \u -> 
		 if maybe False simIsSpecialRemote (M.lookup u (simRepoState st))
		 	then Left $ fromRepoName reponame ++ " is a special remote, so git-annex cannot run on it."
			else a u

checkKnownRemote :: RemoteName -> RepoName -> UUID -> SimState SimRepo -> (UUID -> Either String a) -> Either String a
checkKnownRemote remotename reponame u st a =
	let rs = fromMaybe mempty $ M.lookup u (simConnections st)
	in if S.member remotename rs
		then checkKnownRepo (remoteNameToRepoName remotename) st a
		else Left $ "Repository " ++ fromRepoName reponame 
			++ " does not have a remote \"" 
			++ fromRemoteName remotename ++ "\"."

checkValidPreferredContentExpression :: [PreferredContentExpression] -> v -> Either String v
checkValidPreferredContentExpression [] v = Right v
checkValidPreferredContentExpression (expr:rest) v =
	case checkPreferredContentExpression expr of
		Nothing -> checkValidPreferredContentExpression rest v
		Just e -> Left $ "Failed parsing \"" ++ expr ++ "\": " ++ e

simRandom :: SimState t -> (StdGen -> (v, StdGen)) -> (v -> r) -> (r, SimState t)
simRandom st mk f =
	let rng = mkStdGen (simRng st)
	    (v, rng') = mk rng
	    (newseed, _) = random rng'
	in (f v, st { simRng = newseed })

randomPreferredContentExpression :: SimState SimRepo -> [String] -> ((PreferredContentExpression, SimState SimRepo) -> t) -> t
randomPreferredContentExpression st terms f = 
	f (simRandom st (randomPreferredContentExpression' terms) id)

randomPreferredContentExpression' :: [String] -> StdGen -> (PreferredContentExpression, StdGen)
randomPreferredContentExpression' terms rng = 
	let (n, rng') = randomR (1, nterms) rng
	in go [] n rng'
  where
	go c 0 rng' = (unwords (concat c), rng')
	go c n rng' = 
		let (idx, rng'') = randomR (0, nterms - 1) rng'
		    term = terms !! idx
		    (notted, rng''') = random rng''
		    (combineand, rng'''') = random rng'''
		    combiner = if null c
		    	then []
			else if combineand then ["and"] else ["or"]
		    subexpr = if notted 
		    	then "not":term:combiner
		    	else term:combiner
		in go (subexpr:c) (pred n) rng''''
	nterms = length terms

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
		, simLiveSizeChanges = mempty
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
updateSimRepoStates = overSimRepoStates updateSimRepoState

quiesceSim :: SimState SimRepo -> IO (SimState SimRepo)
quiesceSim = overSimRepoStates go
  where
	go st sr = do
		((), astrd) <- Annex.run (simRepoAnnex sr) $ doQuietAction $
			quiesce False
		return $ sr
			{ simRepoAnnex = astrd
			, simRepoCurrState = st
			}

overSimRepoStates :: (SimState SimRepo -> SimRepo -> IO SimRepo) -> SimState SimRepo -> IO (SimState SimRepo)
overSimRepoStates a inst = go inst (M.toList $ simRepoState inst)
  where
	go st [] = return st
	go st ((u, rst):rest) = case simRepo rst of
		Just sr -> do
			sr' <- a st sr
			let rst' = rst { simRepo = Just sr' }
			let st' = st
				{ simRepoState = M.insert u rst'
					(simRepoState st)
				}
			go st' rest
		Nothing -> go st rest

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

runSimRepo :: UUID -> SimState SimRepo -> (SimState SimRepo -> SimRepoState SimRepo -> Annex (SimState SimRepo, t)) -> IO (SimState SimRepo, t)
runSimRepo u st a = do
	st' <- updateSimRepos st
	case M.lookup u (simRepoState st') of
		Just rst -> case simRepo rst of
			Just sr -> do
				((st'', t), strd) <- Annex.run (simRepoAnnex sr) $
					doQuietAction (a st' rst)
				let sr' = sr
					{ simRepoAnnex = strd
					}
				let st''' = st''
					{ simRepoState = M.adjust
						(\rst' -> rst' { simRepo = Just sr' })
						u
						(simRepoState st'')
					}
				return (st''', t)
			Nothing -> error $ "runSimRepo simRepo not set for " ++ fromUUID u
		Nothing -> error $ "runSimRepo simRepoState not found for " ++ fromUUID u

rememberLiveSizeChanges :: Bool -> UUID -> Key -> SimRepoState t -> SimRepoState t -> SimRepoState t
rememberLiveSizeChanges present u k oldrst newrst
	| u `S.member` getSimLocations oldrst k == present = newrst
	| otherwise = 
		let m = M.alter go u (simLiveSizeChanges newrst)
		in newrst { simLiveSizeChanges = m }
  where
	ksz = fromMaybe 0 $ fromKey keySize k
	change
		| present = SizeOffset ksz
		| otherwise = SizeOffset (negate ksz)

	go Nothing = Just change
	go (Just oldoffset) = Just (oldoffset + change)

calcLiveSizeChanges :: SimRepoState t -> SimRepoState t
calcLiveSizeChanges rst = rst
	{ simLiveSizeChanges = go mempty $ M.toList $ simLocations rst
	}
  where
	go m [] = m
	go m ((k, locm):rest) = go (go' k m (M.toList locm)) rest

	go' _ m [] = m
	go' k m ((u, locst):rest) = go' k (M.alter (calc k locst) u m) rest

	calc k (LocationState _ present) msz
		| not present = msz
		| otherwise = Just $ fromMaybe (SizeOffset 0) msz + 
			SizeOffset (fromMaybe 0 $ fromKey keySize k)

{- Update the RepoSize database in a simulated repository as if LiveUpdate
 - were done for the simulated changes in keys locations that have occurred
 - in the simulation up to this point.
 - 
 - This relies on the SizeChanges table being a rolling total. When the
 - simulation is suspended, the location logs get updated with changes
 - corresponding to the size changes recorded here. When the simulation is
 - later resumed, the values written here are taken as the start values
 - for the rolling total, and so getLiveRepoSizes will only see the
 - difference between that start value and whatever new value is written
 - here.
 -
 - This assumes that the simulation is not interrupted after calling
 - this, but before it can update the location logs.
 -}
updateLiveSizeChanges :: SimRepoState t -> Annex a -> Annex a
updateLiveSizeChanges rst a = do
	h <- Database.RepoSize.getRepoSizeHandle
	liftIO $ Database.RepoSize.setSizeChanges h $
		M.map fromSizeOffset $ simLiveSizeChanges rst
	a

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
		liftIO $ createDirectoryIfMissing True $
			takeDirectory $ fromRawFilePath f'
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
	st' <- quiesceSim =<< updateSimRepos st
	let st'' = st'
		{ simRepoState = M.map freeze (simRepoState st')
		}
	writeFile (simRootDirectory st'' </> "state") (show st'')
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
				let st' = st { simRootDirectory = fromRawFilePath rootdir }
				repostate <- M.fromList
					<$> mapM (thaw st') (M.toList (simRepoState st))
				let st'' = st'
					{ simRepoState = 
						M.map (finishthaw st'') repostate
					}
				return (Right st'')
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

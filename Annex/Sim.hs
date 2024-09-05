{- git-annex simulator
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Sim where

import Utility.DataUnits
import Types.NumCopies
import Types.FileMatcher
import Types.RepoSize
import Types.Key
import Types.UUID
import Annex (Annex)
import Backend.Hash (genTestKey)
import Annex.UUID
import Utility.FileSystemEncoding
import qualified Remote

import System.Random
import Data.Word
import Data.Maybe
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
	, simSpecialRemotes :: M.Map RepoName UUID
	, simConnections :: M.Map RepoName (S.Set RepoName)
	, simFiles :: M.Map FilePath Key
	, simRng :: StdGen
	, simNumCopies :: NumCopies
	, simGroups :: M.Map RepoName (S.Set GroupName)
	, simWanted :: M.Map RepoName Matcher
	, simRequired :: M.Map RepoName Matcher
	, simGroupWanted :: M.Map GroupName Matcher
	, simMaxSize :: M.Map RepoName MaxSize
	, simRebalance :: Bool
	, simExistingRepoByName :: ExistingRepoByName
	}
	deriving (Show)

emptySimState :: Int -> ExistingRepoByName -> SimState
emptySimState rngseed repobyname = SimState
	{ simRepos = mempty
	, simSpecialRemotes = mempty
	, simRepoState = mempty
	, simConnections = mempty
	, simFiles = mempty
	, simRng = mkStdGen rngseed
	, simNumCopies = configuredNumCopies 1
	, simGroups = mempty
	, simWanted = mempty
	, simRequired = mempty
	, simGroupWanted = mempty
	, simMaxSize = mempty
	, simRebalance = False
	, simExistingRepoByName = repobyname
	}

-- State that can vary between different repos in the simulation.
data SimRepoState = SimRepoState
	{ simLocations :: M.Map Key (S.Set RepoName)
	}
	deriving (Show, Eq)

emptySimRepoState :: SimRepoState
emptySimRepoState = SimRepoState mempty

setPresentKey :: RepoName -> Key -> SimRepoState -> SimRepoState
setPresentKey repo k rst = rst
	{ simLocations = 
		M.insertWith S.union k (S.singleton repo) (simLocations rst)
	}

data Matcher = Matcher String (FileMatcher Annex)

instance Show Matcher where
	show (Matcher s _) = s

newtype RepoName = RepoName { fromRepoName :: String }
	deriving (Show, Eq, Ord)

newtype GroupName = GroupName { fromGroupName :: String }
	deriving (Show, Eq, Ord)

data SimCommand
	= CommandInit RepoName
	| CommandInitRemote RepoName
	| CommandUse RepoName String
	| CommandConnect RepoName RepoName
	| CommandDisconnect RepoName RepoName
	| CommandAddTree RepoName Matcher
	| CommandAdd FilePath ByteSize RepoName
	| CommandStep Int
	| CommandSeed Int
	| CommandPresent RepoName FilePath
	| CommandNotPresent RepoName FilePath
	| CommandNumCopies Int
	| CommandGroup RepoName GroupName
	| CommandUngroup RepoName GroupName
	| CommandWanted RepoName String
	| CommandRequired RepoName String
	| CommandGroupWanted GroupName String
	| CommandMaxSize RepoName MaxSize
	| CommandRebalance Bool
	deriving (Show)

applySimCommand :: SimCommand -> SimState -> Either String SimState
applySimCommand (CommandInit reponame) st =
	let (u, st') = genSimUUID st reponame
	in Right $ st'
		{ simRepos = M.insert reponame u (simRepos st')
		}
applySimCommand (CommandInitRemote reponame) st =
	let (u, st') = genSimUUID st reponame
	in Right $ st'
		{ simSpecialRemotes = M.insert reponame u (simSpecialRemotes st')
		}
applySimCommand (CommandUse reponame s) st =
	case existingRepoByName (simExistingRepoByName st) reponame of
		(u:[], _) -> Right $ st
			{ simSpecialRemotes = M.insert reponame u (simSpecialRemotes st)
			}
		(_, msg) -> Left $ "Unable to use a repository \"" 
			++ fromRepoName reponame 
			++ "\" in the simulation because " ++ msg
applySimCommand (CommandConnect repo remote) st = Right $ st
	{ simConnections = 
		let s = case M.lookup repo (simConnections st) of
			Just s -> S.insert remote s
			Nothing -> S.singleton remote
		in M.insert repo s (simConnections st)
	}
applySimCommand (CommandDisconnect repo remote) st = Right $ st
	{ simConnections = 
		let sc = case M.lookup repo (simConnections st) of
			Just s -> S.delete remote s
			Nothing -> S.empty
		in M.insert repo sc (simConnections st)
	}
applySimCommand (CommandAddTree repo matcher) st = error "TODO" -- XXX
applySimCommand (CommandAdd file sz repo) st = 
	let (k, st') = genSimKey sz st
	in Right $ st'
		{ simFiles = M.insert file k (simFiles st')
		, simRepoState = 
			let rst = fromMaybe emptySimRepoState $
				M.lookup repo (simRepoState st')
			    rst' = setPresentKey repo k rst
			in M.insert repo rst' (simRepoState st')
		}
applySimCommand (CommandStep n) st
	| n > 0 = applySimCommand
		(CommandStep (pred n))
		(fst $ stepSimulation st)
	| otherwise = Right st
applySimCommand (CommandSeed rngseed) st = Right $ st
	{ simRng = mkStdGen rngseed
	}
applySimCommand (CommandPresent repo file) st =
	case (M.lookup file (simFiles st), M.lookup repo (simRepoState st)) of
		(Just k, Just rst) -> case M.lookup k (simLocations rst) of
			Just locs | S.member repo locs -> Right st
			_ -> missing
		(Just k, Nothing) -> missing
		(Nothing, _) -> Left $ "Expected " ++ file
			++ " to be present in " ++ fromRepoName repo 
			++ ", but the simulation does not include that file."
  where
	missing = Left $ "Expected " ++ file ++ " to be present in " 
		++ fromRepoName repo ++ ", but it is not."
applySimCommand (CommandNotPresent repo file) st =
	case (M.lookup file (simFiles st), M.lookup repo (simRepoState st)) of
		(Just k, Just rst) -> case M.lookup k (simLocations rst) of
			Just locs | S.notMember repo locs -> Right st
			_ -> present
		(Just k, Nothing) -> present
		(Nothing, _) -> Left $ "Expected " ++ file
			++ " to not be present in " ++ fromRepoName repo 
			++ ", but the simulation does not include that file."
  where
	present = Left $ "Expected " ++ file ++ " not to be present in " 
		++ fromRepoName repo ++ ", but it is present."
applySimCommand (CommandNumCopies n) st = Right $ st
	{ simNumCopies = configuredNumCopies n
	}
applySimCommand (CommandGroup repo group) st = Right $ st
	{ simGroups = M.insertWith S.union repo (S.singleton group) (simGroups st)
	}
applySimCommand (CommandUngroup repo group) st = Right $ st
	{ simGroups = M.adjust (S.delete group) repo (simGroups st)
	}
applySimCommand (CommandWanted repo expr) st = undefined -- XXX
applySimCommand (CommandRequired repo expr) st = undefined -- XXX
applySimCommand (CommandGroupWanted group expr) st = undefined  -- XXX
applySimCommand (CommandMaxSize repo sz) st = Right $ st
	{ simMaxSize = M.insert repo sz (simMaxSize st)
	}
applySimCommand (CommandRebalance b) st = Right $ st
	{ simRebalance = b
	}

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

newtype ExistingRepoByName = ExistingRepoByName 
	{ existingRepoByName :: RepoName -> ([UUID], String)
	}

instance Show ExistingRepoByName where
	show _ = "ExistingRepoByName"

mkExistingRepoByName :: Annex ExistingRepoByName
mkExistingRepoByName = do
	f <- Remote.nameToUUID''
	return $ ExistingRepoByName $ f . fromRepoName

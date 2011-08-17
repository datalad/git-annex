{- git-annex monad
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex (
	Annex,
	AnnexState(..),
	new,
	run,
	eval,
	getState,
	changeState,
	gitRepo
) where

import Control.Monad.State

import qualified Git
import Git.Queue
import Types.Backend
import Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Types.UUID

-- git-annex's monad
type Annex = StateT AnnexState IO

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [Backend Annex]
	, remotes :: [Remote Annex]
	, repoqueue :: Queue
	, quiet :: Bool
	, force :: Bool
	, fast :: Bool
	, branchstate :: BranchState
	, forcebackend :: Maybe String
	, forcenumcopies :: Maybe Int
	, defaultkey :: Maybe String
	, toremote :: Maybe String
	, fromremote :: Maybe String
	, exclude :: [String]
	, forcetrust :: [(UUID, TrustLevel)]
	, trustmap :: Maybe TrustMap
	, cipher :: Maybe Cipher
	}

newState :: Git.Repo -> AnnexState
newState gitrepo = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, repoqueue = empty
	, quiet = False
	, force = False
	, fast = False
	, branchstate = startBranchState
	, forcebackend = Nothing
	, forcenumcopies = Nothing
	, defaultkey = Nothing
	, toremote = Nothing
	, fromremote = Nothing
	, exclude = []
	, forcetrust = []
	, trustmap = Nothing
	, cipher = Nothing
	}

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> IO AnnexState
new gitrepo = newState `liftM` (liftIO . Git.configRead) gitrepo

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run = flip runStateT
eval :: AnnexState -> Annex a -> IO a
eval = flip evalStateT

{- Gets a value from the internal state, selected by the passed value
 - constructor. -}
getState :: (AnnexState -> a) -> Annex a
getState = gets

{- Applies a state mutation function to change the internal state. 
 -
 - Example: changeState $ \s -> s { quiet = True }
 -}
changeState :: (AnnexState -> AnnexState) -> Annex ()
changeState = modify

{- Returns the git repository being acted on -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo

{- git-annex monad
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Annex (
	Annex,
	AnnexState(..),
	OutputType(..),
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
newtype Annex a = Annex { runAnnex :: StateT AnnexState IO a }
	deriving (
		Monad,
		MonadIO,
		MonadState AnnexState,
		Functor
	)

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [Backend Annex]
	, remotes :: [Remote Annex]
	, repoqueue :: Queue
	, output :: OutputType
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

data OutputType = NormalOutput | QuietOutput | JSONOutput

newState :: Git.Repo -> AnnexState
newState gitrepo = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, repoqueue = empty
	, output = NormalOutput
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
new gitrepo = newState `liftM` Git.configRead gitrepo

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run s a = runStateT (runAnnex a) s
eval :: AnnexState -> Annex a -> IO a
eval s a = evalStateT (runAnnex a) s

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

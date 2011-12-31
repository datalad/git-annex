{- git-annex monad
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses #-}

module Annex (
	Annex,
	AnnexState(..),
	OutputType(..),
	new,
	newState,
	run,
	eval,
	getState,
	changeState,
	gitRepo,
	inRepo,
	fromRepo,
) where

import Control.Monad.State

import Common
import qualified Git
import qualified Git.Config
import Git.CatFile
import qualified Git.Queue
import Types.Backend
import qualified Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Types.UUID
import qualified Utility.Matcher
import qualified Utility.Format
import qualified Data.Map as M

-- needed for Debian stable's haskell to derive Applicative for StateT
instance (Functor m, Monad m) => Applicative (StateT s m) where
	pure = return
	(<*>) = ap

-- git-annex's monad
newtype Annex a = Annex { runAnnex :: StateT AnnexState IO a }
	deriving (
		Monad,
		MonadIO,
		MonadState AnnexState,
		Functor,
		Applicative
	)

data OutputType = NormalOutput | QuietOutput | JSONOutput

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [Backend Annex]
	, remotes :: [Types.Remote.Remote Annex]
	, repoqueue :: Git.Queue.Queue
	, output :: OutputType
	, force :: Bool
	, fast :: Bool
	, auto :: Bool
	, format :: Maybe Utility.Format.Format
	, branchstate :: BranchState
	, catfilehandle :: Maybe CatFileHandle
	, forcebackend :: Maybe String
	, forcenumcopies :: Maybe Int
	, toremote :: Maybe String
	, fromremote :: Maybe String
	, limit :: Either [Utility.Matcher.Token (FilePath -> Annex Bool)] (Utility.Matcher.Matcher (FilePath -> Annex Bool))
	, forcetrust :: [(UUID, TrustLevel)]
	, trustmap :: Maybe TrustMap
	, ciphers :: M.Map EncryptedCipher Cipher
	}

newState :: Git.Repo -> AnnexState
newState gitrepo = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, repoqueue = Git.Queue.new
	, output = NormalOutput
	, force = False
	, fast = False
	, auto = False
	, format = Nothing
	, branchstate = startBranchState
	, catfilehandle = Nothing
	, forcebackend = Nothing
	, forcenumcopies = Nothing
	, toremote = Nothing
	, fromremote = Nothing
	, limit = Left []
	, forcetrust = []
	, trustmap = Nothing
	, ciphers = M.empty
	}

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> IO AnnexState
new gitrepo = newState <$> Git.Config.read gitrepo

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
 - Example: changeState $ \s -> s { output = QuietOutput }
 -}
changeState :: (AnnexState -> AnnexState) -> Annex ()
changeState = modify

{- Returns the annex's git repository. -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo

{- Runs an IO action in the annex's git repository. -}
inRepo :: (Git.Repo -> IO a) -> Annex a
inRepo a = liftIO . a =<< gitRepo

{- Extracts a value from the annex's git repisitory. -}
fromRepo :: (Git.Repo -> a) -> Annex a
fromRepo a = a <$> gitRepo

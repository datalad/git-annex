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
	setFlag,
	setField,
	getFlag,
	getField,
	addCleanup,
	gitRepo,
	inRepo,
	fromRepo,
) where

import Control.Monad.State.Strict
import System.Posix.Types (Fd)

import Common
import qualified Git
import qualified Git.Config
import Git.CatFile
import Git.CheckAttr
import qualified Git.Queue
import Types.Backend
import qualified Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Utility.State
import qualified Utility.Matcher
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

type Matcher a = Either [Utility.Matcher.Token a] (Utility.Matcher.Matcher a)

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, backends :: [BackendA Annex]
	, remotes :: [Types.Remote.RemoteA Annex]
	, output :: OutputType
	, force :: Bool
	, fast :: Bool
	, auto :: Bool
	, branchstate :: BranchState
	, repoqueue :: Maybe Git.Queue.Queue
	, catfilehandle :: Maybe CatFileHandle
	, checkattrhandle :: Maybe CheckAttrHandle
	, forcebackend :: Maybe String
	, forcenumcopies :: Maybe Int
	, limit :: Matcher (FilePath -> Annex Bool)
	, forcetrust :: TrustMap
	, trustmap :: Maybe TrustMap
	, ciphers :: M.Map EncryptedCipher Cipher
	, lockpool :: M.Map FilePath Fd
	, flags :: M.Map String Bool
	, fields :: M.Map String String
	, cleanup :: M.Map String (Annex ())
	}

newState :: Git.Repo -> AnnexState
newState gitrepo = AnnexState
	{ repo = gitrepo
	, backends = []
	, remotes = []
	, output = NormalOutput
	, force = False
	, fast = False
	, auto = False
	, branchstate = startBranchState
	, repoqueue = Nothing
	, catfilehandle = Nothing
	, checkattrhandle = Nothing
	, forcebackend = Nothing
	, forcenumcopies = Nothing
	, limit = Left []
	, forcetrust = M.empty
	, trustmap = Nothing
	, ciphers = M.empty
	, lockpool = M.empty
	, flags = M.empty
	, fields = M.empty
	, cleanup = M.empty
	}

{- Create and returns an Annex state object for the specified git repo. -}
new :: Git.Repo -> IO AnnexState
new gitrepo = newState <$> Git.Config.read gitrepo

{- performs an action in the Annex monad -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run s a = runStateT (runAnnex a) s
eval :: AnnexState -> Annex a -> IO a
eval s a = evalStateT (runAnnex a) s

{- Sets a flag to True -}
setFlag :: String -> Annex ()
setFlag flag = changeState $ \s ->
	s { flags = M.insertWith' const flag True $ flags s }

{- Sets a field to a value -}
setField :: String -> String -> Annex ()
setField field value = changeState $ \s ->
	s { fields = M.insertWith' const field value $ fields s }

{- Adds a cleanup action to perform. -}
addCleanup :: String -> Annex () -> Annex ()
addCleanup uid a = changeState $ \s ->
	s { cleanup = M.insertWith' const uid a $ cleanup s }

{- Checks if a flag was set. -}
getFlag :: String -> Annex Bool
getFlag flag = fromMaybe False . M.lookup flag <$> getState flags

{- Gets the value of a field. -}
getField :: String -> Annex (Maybe String)
getField field = M.lookup field <$> getState fields

{- Returns the annex's git repository. -}
gitRepo :: Annex Git.Repo
gitRepo = getState repo

{- Runs an IO action in the annex's git repository. -}
inRepo :: (Git.Repo -> IO a) -> Annex a
inRepo a = liftIO . a =<< gitRepo

{- Extracts a value from the annex's git repisitory. -}
fromRepo :: (Git.Repo -> a) -> Annex a
fromRepo a = a <$> gitRepo

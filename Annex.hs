{- git-annex monad
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving, PackageImports #-}

module Annex (
	Annex,
	AnnexState(..),
	PreferredContentMap,
	new,
	run,
	eval,
	getState,
	changeState,
	setFlag,
	setField,
	setOutput,
	getFlag,
	getField,
	addCleanup,
	gitRepo,
	inRepo,
	fromRepo,
	calcRepo,
	getGitConfig,
	changeGitConfig,
	changeGitRepo,
	withCurrentState,
) where

import "mtl" Control.Monad.Reader
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import System.Posix.Types (Fd)
import Control.Concurrent

import Common
import qualified Git
import qualified Git.Config
import Annex.Direct.Fixup
import Git.CatFile
import Git.CheckAttr
import Git.CheckIgnore
import Git.SharedRepository
import qualified Git.Queue
import Types.Backend
import Types.GitConfig
import qualified Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Types.Group
import Types.Messages
import Types.UUID
import Types.FileMatcher
import qualified Utility.Matcher
import qualified Data.Map as M
import qualified Data.Set as S

{- git-annex's monad is a ReaderT around an AnnexState stored in a MVar.
 - This allows modifying the state in an exception-safe fashion.
 - The MVar is not exposed outside this module.
 -}
newtype Annex a = Annex { runAnnex :: ReaderT (MVar AnnexState) IO a }
	deriving (
		Monad,
		MonadIO,
		MonadReader (MVar AnnexState),
		MonadCatchIO,
		Functor,
		Applicative
	)

type Matcher a = Either [Utility.Matcher.Token a] (Utility.Matcher.Matcher a)
type PreferredContentMap = M.Map UUID (Utility.Matcher.Matcher (S.Set UUID -> MatchInfo -> Annex Bool))

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, gitconfig :: GitConfig
	, backends :: [BackendA Annex]
	, remotes :: [Types.Remote.RemoteA Annex]
	, output :: MessageState
	, force :: Bool
	, fast :: Bool
	, auto :: Bool
	, daemon :: Bool
	, branchstate :: BranchState
	, repoqueue :: Maybe Git.Queue.Queue
	, catfilehandles :: M.Map FilePath CatFileHandle
	, checkattrhandle :: Maybe CheckAttrHandle
	, checkignorehandle :: Maybe (Maybe CheckIgnoreHandle)
	, forcebackend :: Maybe String
	, globalnumcopies :: Maybe Int
	, limit :: Matcher (MatchInfo -> Annex Bool)
	, uuidmap :: Maybe UUIDMap
	, preferredcontentmap :: Maybe PreferredContentMap
	, shared :: Maybe SharedRepository
	, forcetrust :: TrustMap
	, trustmap :: Maybe TrustMap
	, groupmap :: Maybe GroupMap
	, ciphers :: M.Map StorableCipher Cipher
	, lockpool :: M.Map FilePath Fd
	, flags :: M.Map String Bool
	, fields :: M.Map String String
	, cleanup :: M.Map String (Annex ())
	, inodeschanged :: Maybe Bool
	, useragent :: Maybe String
	, errcounter :: Integer
	}

newState :: GitConfig -> Git.Repo -> AnnexState
newState c r = AnnexState
	{ repo = r
	, gitconfig = c
	, backends = []
	, remotes = []
	, output = defaultMessageState
	, force = False
	, fast = False
	, auto = False
	, daemon = False
	, branchstate = startBranchState
	, repoqueue = Nothing
	, catfilehandles = M.empty
	, checkattrhandle = Nothing
	, checkignorehandle = Nothing
	, forcebackend = Nothing
	, globalnumcopies = Nothing
	, limit = Left []
	, uuidmap = Nothing
	, preferredcontentmap = Nothing
	, shared = Nothing
	, forcetrust = M.empty
	, trustmap = Nothing
	, groupmap = Nothing
	, ciphers = M.empty
	, lockpool = M.empty
	, flags = M.empty
	, fields = M.empty
	, cleanup = M.empty
	, inodeschanged = Nothing
	, useragent = Nothing
	, errcounter = 0
	}

{- Makes an Annex state object for the specified git repo.
 - Ensures the config is read, if it was not already. -}
new :: Git.Repo -> IO AnnexState
new r = do
	r' <- Git.Config.read r
	let c = extractGitConfig r'
	newState c <$> if annexDirect c then fixupDirect r' else return r'

{- Performs an action in the Annex monad from a starting state,
 - returning a new state. -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run s a = do
	mvar <- newMVar s
	r <- runReaderT (runAnnex a) mvar
	s' <- takeMVar mvar
	return (r, s')

{- Performs an action in the Annex monad from a starting state, 
 - and throws away the new state. -}
eval :: AnnexState -> Annex a -> IO a
eval s a = do
	mvar <- newMVar s
	runReaderT (runAnnex a) mvar

getState :: (AnnexState -> v) -> Annex v
getState selector = do
	mvar <- ask
	s <- liftIO $ readMVar mvar
	return $ selector s

changeState :: (AnnexState -> AnnexState) -> Annex ()
changeState modifier = do
	mvar <- ask
	liftIO $ modifyMVar_ mvar $ return . modifier

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

{- Sets the type of output to emit. -}
setOutput :: OutputType -> Annex ()
setOutput o = changeState $ \s ->
	s { output = (output s) { outputType = o } }

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

{- Calculates a value from an annex's git repository and its GitConfig. -}
calcRepo :: (Git.Repo -> GitConfig -> IO a) -> Annex a
calcRepo a = do
	s <- getState id
	liftIO $ a (repo s) (gitconfig s)

{- Gets the GitConfig settings. -}
getGitConfig :: Annex GitConfig
getGitConfig = getState gitconfig

{- Modifies a GitConfig setting. -}
changeGitConfig :: (GitConfig -> GitConfig) -> Annex ()
changeGitConfig a = changeState $ \s -> s { gitconfig = a (gitconfig s) }

{- Changing the git Repo data also involves re-extracting its GitConfig. -}
changeGitRepo :: Git.Repo -> Annex ()
changeGitRepo r = changeState $ \s -> s
	{ repo = r
	, gitconfig = extractGitConfig r
	}

{- Converts an Annex action into an IO action, that runs with a copy
 - of the current Annex state. 
 -
 - Use with caution; the action should not rely on changing the
 - state, as it will be thrown away. -}
withCurrentState :: Annex a -> Annex (IO a)
withCurrentState a = do
	s <- getState id
	return $ eval s a

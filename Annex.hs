{- git-annex monad
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Annex (
	Annex,
	AnnexState(..),
	new,
	run,
	eval,
	makeRunner,
	getState,
	changeState,
	withState,
	setFlag,
	setField,
	setOutput,
	getFlag,
	getField,
	addCleanupAction,
	gitRepo,
	inRepo,
	fromRepo,
	calcRepo,
	getGitConfig,
	overrideGitConfig,
	changeGitRepo,
	adjustGitRepo,
	addGitConfigOverride,
	getGitConfigOverrides,
	getRemoteGitConfig,
	withCurrentState,
	changeDirectory,
	getGitRemotes,
	incError,
) where

import Common
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import Annex.Fixup
import Git.HashObject
import Git.CheckAttr
import Git.CheckIgnore
import qualified Git.Hook
import qualified Git.Queue
import Types.Key
import Types.Backend
import Types.GitConfig
import qualified Types.Remote
import Types.Crypto
import Types.BranchState
import Types.TrustLevel
import Types.Group
import Types.Messages
import Types.Concurrency
import Types.UUID
import Types.FileMatcher
import Types.NumCopies
import Types.LockCache
import Types.DesktopNotify
import Types.CleanupActions
import Types.AdjustedBranch
import Types.WorkerPool
import Types.IndexFiles
import Types.CatFileHandles
import Types.RemoteConfig
import Types.TransferrerPool
import Types.VectorClock
import Annex.VectorClock.Utility
import qualified Database.Keys.Handle as Keys
import Utility.InodeCache
import Utility.Url
import Utility.ResourcePool
import Utility.HumanTime

import "mtl" Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Monad.Fail as Fail
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time.Clock.POSIX

{- git-annex's monad is a ReaderT around an AnnexState stored in a MVar.
 - The MVar is not exposed outside this module.
 -
 - Note that when an Annex action fails and the exception is caught,
 - any changes the action has made to the AnnexState are retained,
 - due to the use of the MVar to store the state.
 -}
newtype Annex a = Annex { runAnnex :: ReaderT (MVar AnnexState) IO a }
	deriving (
		Monad,
		MonadIO,
		MonadReader (MVar AnnexState),
		MonadCatch,
		MonadThrow,
		MonadMask,
		Fail.MonadFail,
		Functor,
		Applicative,
		Alternative
	)

-- internal state storage
data AnnexState = AnnexState
	{ repo :: Git.Repo
	, repoadjustment :: (Git.Repo -> IO Git.Repo)
	, gitconfig :: GitConfig
	, gitconfigadjustment :: (GitConfig -> GitConfig)
	, gitconfigoverride :: [String]
	, gitremotes :: Maybe [Git.Repo]
	, backend :: Maybe (BackendA Annex)
	, remotes :: [Types.Remote.RemoteA Annex]
	, output :: MessageState
	, concurrency :: ConcurrencySetting
	, force :: Bool
	, fast :: Bool
	, daemon :: Bool
	, branchstate :: BranchState
	, getvectorclock :: IO VectorClock
	, repoqueue :: Maybe (Git.Queue.Queue Annex)
	, catfilehandles :: CatFileHandles
	, hashobjecthandle :: Maybe HashObjectHandle
	, checkattrhandle :: Maybe (ResourcePool CheckAttrHandle)
	, checkignorehandle :: Maybe (ResourcePool CheckIgnoreHandle)
	, forcebackend :: Maybe String
	, globalnumcopies :: Maybe NumCopies
	, globalmincopies :: Maybe MinCopies
	, forcenumcopies :: Maybe NumCopies
	, forcemincopies :: Maybe MinCopies
	, limit :: ExpandableMatcher Annex
	, timelimit :: Maybe (Duration, POSIXTime)
	, uuiddescmap :: Maybe UUIDDescMap
	, preferredcontentmap :: Maybe (FileMatcherMap Annex)
	, requiredcontentmap :: Maybe (FileMatcherMap Annex)
	, remoteconfigmap :: Maybe (M.Map UUID RemoteConfig)
	, forcetrust :: TrustMap
	, trustmap :: Maybe TrustMap
	, groupmap :: Maybe GroupMap
	, ciphers :: M.Map StorableCipher Cipher
	, lockcache :: LockCache
	, sshstalecleaned :: TMVar Bool
	, flags :: M.Map String Bool
	, fields :: M.Map String String
	, cleanupactions :: M.Map CleanupAction (Annex ())
	, signalactions :: TVar (M.Map SignalAction (Int -> IO ()))
	, sentinalstatus :: Maybe SentinalStatus
	, useragent :: Maybe String
	, errcounter :: Integer
	, adjustedbranchrefreshcounter :: Integer
	, unusedkeys :: Maybe (S.Set Key)
	, tempurls :: M.Map Key URLString
	, existinghooks :: M.Map Git.Hook.Hook Bool
	, desktopnotify :: DesktopNotify
	, workers :: Maybe (TMVar (WorkerPool AnnexState))
	, activekeys :: TVar (M.Map Key ThreadId)
	, activeremotes :: MVar (M.Map (Types.Remote.RemoteA Annex) Integer)
	, keysdbhandle :: Keys.DbHandle
	, cachedcurrentbranch :: (Maybe (Maybe Git.Branch, Maybe Adjustment))
	, cachedgitenv :: Maybe (AltIndexFile, FilePath, [(String, String)])
	, urloptions :: Maybe UrlOptions
	, insmudgecleanfilter :: Bool
	, transferrerpool :: TransferrerPool
	}

newState :: GitConfig -> Git.Repo -> IO AnnexState
newState c r = do
	emptyactiveremotes <- newMVar M.empty
	emptyactivekeys <- newTVarIO M.empty
	si <- newTVarIO M.empty
	o <- newMessageState
	sc <- newTMVarIO False
	kh <- Keys.newDbHandle
	tp <- newTransferrerPool
	vc <- startVectorClock
	return $ AnnexState
		{ repo = r
		, repoadjustment = return
		, gitconfig = c
		, gitconfigadjustment = id
		, gitconfigoverride = []
		, gitremotes = Nothing
		, backend = Nothing
		, remotes = []
		, output = o
		, concurrency = ConcurrencyCmdLine NonConcurrent
		, force = False
		, fast = False
		, daemon = False
		, branchstate = startBranchState
		, getvectorclock = vc
		, repoqueue = Nothing
		, catfilehandles = catFileHandlesNonConcurrent
		, hashobjecthandle = Nothing
		, checkattrhandle = Nothing
		, checkignorehandle = Nothing
		, forcebackend = Nothing
		, globalnumcopies = Nothing
		, globalmincopies = Nothing
		, forcenumcopies = Nothing
		, forcemincopies = Nothing
		, limit = BuildingMatcher []
		, timelimit = Nothing
		, uuiddescmap = Nothing
		, preferredcontentmap = Nothing
		, requiredcontentmap = Nothing
		, remoteconfigmap = Nothing
		, forcetrust = M.empty
		, trustmap = Nothing
		, groupmap = Nothing
		, ciphers = M.empty
		, lockcache = M.empty
		, sshstalecleaned = sc
		, flags = M.empty
		, fields = M.empty
		, cleanupactions = M.empty
		, signalactions = si
		, sentinalstatus = Nothing
		, useragent = Nothing
		, errcounter = 0
		, adjustedbranchrefreshcounter = 0
		, unusedkeys = Nothing
		, tempurls = M.empty
		, existinghooks = M.empty
		, desktopnotify = mempty
		, workers = Nothing
		, activekeys = emptyactivekeys
		, activeremotes = emptyactiveremotes
		, keysdbhandle = kh
		, cachedcurrentbranch = Nothing
		, cachedgitenv = Nothing
		, urloptions = Nothing
		, insmudgecleanfilter = False
		, transferrerpool = tp
		}

{- Makes an Annex state object for the specified git repo.
 - Ensures the config is read, if it was not already, and performs
 - any necessary git repo fixups. -}
new :: Git.Repo -> IO AnnexState
new r = do
	r' <- Git.Config.read =<< Git.relPath r
	let c = extractGitConfig FromGitConfig r'
	newState c =<< fixupRepo r' c

{- Performs an action in the Annex monad from a starting state,
 - returning a new state. -}
run :: AnnexState -> Annex a -> IO (a, AnnexState)
run s a = flip run' a =<< newMVar s

run' :: MVar AnnexState -> Annex a -> IO (a, AnnexState)
run' mvar a = do
	r <- runReaderT (runAnnex a) mvar
		`onException` (flush =<< readMVar mvar)
	s' <- takeMVar mvar
	flush s'
	return (r, s')
  where
	flush = Keys.flushDbQueue . keysdbhandle

{- Performs an action in the Annex monad from a starting state, 
 - and throws away the new state. -}
eval :: AnnexState -> Annex a -> IO a
eval s a = fst <$> run s a

{- Makes a runner action, that allows diving into IO and from inside
 - the IO action, running an Annex action. -}
makeRunner :: Annex (Annex a -> IO a)
makeRunner = do
	mvar <- ask
	return $ \a -> do
		(r, s) <- run' mvar a
		putMVar mvar s
		return r

getState :: (AnnexState -> v) -> Annex v
getState selector = do
	mvar <- ask
	s <- liftIO $ readMVar mvar
	return $ selector s

changeState :: (AnnexState -> AnnexState) -> Annex ()
changeState modifier = do
	mvar <- ask
	liftIO $ modifyMVar_ mvar $ return . modifier

withState :: (AnnexState -> IO (AnnexState, b)) -> Annex b
withState modifier = do
	mvar <- ask
	liftIO $ modifyMVar mvar modifier

{- Sets a flag to True -}
setFlag :: String -> Annex ()
setFlag flag = changeState $ \s ->
	s { flags = M.insert flag True $ flags s }

{- Sets a field to a value -}
setField :: String -> String -> Annex ()
setField field value = changeState $ \s ->
	s { fields = M.insert field value $ fields s }

{- Adds a cleanup action to perform. -}
addCleanupAction :: CleanupAction -> Annex () -> Annex ()
addCleanupAction k a = changeState $ \s ->
	s { cleanupactions = M.insert k a $ cleanupactions s }

{- Sets the type of output to emit. -}
setOutput :: OutputType -> Annex ()
setOutput o = changeState $ \s ->
	let m = output s
	in s { output = m { outputType = adjustOutputType (outputType m) o } }

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

{- Overrides a GitConfig setting. The modification persists across
 - reloads of the repo's config. -}
overrideGitConfig :: (GitConfig -> GitConfig) -> Annex ()
overrideGitConfig f = changeState $ \s -> s
	{ gitconfigadjustment = gitconfigadjustment s . f
	, gitconfig = f (gitconfig s)
	}

{- Adds an adjustment to the Repo data. Adjustments persist across reloads
 - of the repo's config.
 -
 - Note that the action may run more than once, and should avoid eg,
 - appending the same value to a repo's config when run repeatedly.
 -}
adjustGitRepo :: (Git.Repo -> IO Git.Repo) -> Annex ()
adjustGitRepo a = do
	changeState $ \s -> s { repoadjustment = \r -> repoadjustment s r >>= a }
	changeGitRepo =<< gitRepo

{- Adds git config setting, like "foo=bar". It will be passed with -c
 - to git processes. The config setting is also recorded in the Repo,
 - and the GitConfig is updated. -}
addGitConfigOverride :: String -> Annex ()
addGitConfigOverride v = do
	adjustGitRepo $ \r ->
		Git.Config.store (encodeBS' v) Git.Config.ConfigList $
			r { Git.gitGlobalOpts = go (Git.gitGlobalOpts r) }
	changeState $ \s -> s { gitconfigoverride = v : gitconfigoverride s }
  where
	-- Remove any prior occurrance of the setting to avoid
	-- building up many of them when the adjustment is run repeatedly,
	-- and add the setting to the end.
	go [] = [Param "-c", Param v]
	go (Param "-c": Param v':rest) | v' == v = go rest
	go (c:rest) = c : go rest

{- Values that were passed to addGitConfigOverride. -}
getGitConfigOverrides :: Annex [String]
getGitConfigOverrides = reverse <$> getState gitconfigoverride

{- Changing the git Repo data also involves re-extracting its GitConfig. -}
changeGitRepo :: Git.Repo -> Annex ()
changeGitRepo r = do
	repoadjuster <- getState repoadjustment
	gitconfigadjuster <- getState gitconfigadjustment
	r' <- liftIO $ repoadjuster r
	changeState $ \s -> s
		{ repo = r'
		, gitconfig = gitconfigadjuster $
			extractGitConfig FromGitConfig r'
		}

{- Gets the RemoteGitConfig from a remote, given the Git.Repo for that
 - remote. -}
getRemoteGitConfig :: Git.Repo -> Annex RemoteGitConfig
getRemoteGitConfig r = do
	g <- gitRepo
	liftIO $ atomically $ extractRemoteGitConfig g (Git.repoDescribe r)

{- Converts an Annex action into an IO action, that runs with a copy
 - of the current Annex state. 
 -
 - Use with caution; the action should not rely on changing the
 - state, as it will be thrown away. -}
withCurrentState :: Annex a -> Annex (IO a)
withCurrentState a = do
	s <- getState id
	return $ eval s a

{- It's not safe to use setCurrentDirectory in the Annex monad,
 - because the git repo paths are stored relative.
 - Instead, use this.
 -}
changeDirectory :: FilePath -> Annex ()
changeDirectory d = do
	r <- liftIO . Git.adjustPath absPath =<< gitRepo
	liftIO $ setCurrentDirectory d
	r' <- liftIO $ Git.relPath r
	changeState $ \s -> s { repo = r' }

incError :: Annex ()
incError = changeState $ \s -> 
	let !c = errcounter s + 1 
	    !s' = s { errcounter = c }
	in s'

getGitRemotes :: Annex [Git.Repo]
getGitRemotes = do
	s <- getState id
	case gitremotes s of
		Just rs -> return rs
		Nothing -> do
			rs <- liftIO $ Git.Construct.fromRemotes (repo s)
			changeState $ \s' -> s' { gitremotes = Just rs }
			return rs

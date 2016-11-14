{- git-annex assistant monad
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Assistant.Monad (
	Assistant,
	AssistantData(..),
	newAssistantData,
	runAssistant,
	getAssistant,
	LiftAnnex,
	liftAnnex,
	(<~>),
	(<<~),
	asIO,
	asIO1,
	asIO2,
	ThreadName,
	debug,
	notice
) where

import "mtl" Control.Monad.Reader
import System.Log.Logger

import Annex.Common
import Assistant.Types.ThreadedMonad
import Assistant.Types.DaemonStatus
import Assistant.Types.ScanRemotes
import Assistant.Types.TransferQueue
import Assistant.Types.TransferSlots
import Assistant.Types.TransferrerPool
import Assistant.Types.Pushes
import Assistant.Types.BranchChange
import Assistant.Types.Commits
import Assistant.Types.Changes
import Assistant.Types.RepoProblem
import Assistant.Types.ThreadName
import Assistant.Types.RemoteControl
import Assistant.Types.CredPairCache

newtype Assistant a = Assistant { mkAssistant :: ReaderT AssistantData IO a }
	deriving (
		Monad,
		MonadIO,
		MonadReader AssistantData,
		Functor,
		Applicative
	)

data AssistantData = AssistantData
	{ threadName :: ThreadName
	, threadState :: ThreadState
	, daemonStatusHandle :: DaemonStatusHandle
	, scanRemoteMap :: ScanRemoteMap
	, transferQueue :: TransferQueue
	, transferSlots :: TransferSlots
	, transferrerPool :: TransferrerPool
	, failedPushMap :: FailedPushMap
	, commitChan :: CommitChan
	, changePool :: ChangePool
	, repoProblemChan :: RepoProblemChan
	, branchChangeHandle :: BranchChangeHandle
	, remoteControl :: RemoteControl
	, credPairCache :: CredPairCache
	}

newAssistantData :: ThreadState -> DaemonStatusHandle -> IO AssistantData
newAssistantData st dstatus = AssistantData
	<$> pure (ThreadName "main")
	<*> pure st
	<*> pure dstatus
	<*> newScanRemoteMap
	<*> newTransferQueue
	<*> newTransferSlots
	<*> newTransferrerPool (checkNetworkConnections dstatus)
	<*> newFailedPushMap
	<*> newCommitChan
	<*> newChangePool
	<*> newRepoProblemChan
	<*> newBranchChangeHandle
	<*> newRemoteControl
	<*> newCredPairCache

runAssistant :: AssistantData -> Assistant a -> IO a
runAssistant d a = runReaderT (mkAssistant a) d

getAssistant :: (AssistantData -> a) -> Assistant a
getAssistant = reader

{- Using a type class for lifting into the annex monad allows
 - easily lifting to it from multiple different monads. -}
class LiftAnnex m where
	liftAnnex :: Annex a -> m a

{- Runs an action in the git-annex monad. Note that the same monad state
 - is shared among all assistant threads, so only one of these can run at
 - a time. Therefore, long-duration actions should be avoided. -}
instance LiftAnnex Assistant where
	liftAnnex a = do
		st <- reader threadState
		liftIO $ runThreadState st a

{- Runs an IO action, passing it an IO action that runs an Assistant action. -}
(<~>) :: (IO a -> IO b) -> Assistant a -> Assistant b
io <~> a = do
	d <- reader id
	liftIO $ io $ runAssistant d a

{- Creates an IO action that will run an Assistant action when run. -}
asIO :: Assistant a -> Assistant (IO a)
asIO a = do
	d <- reader id
	return $ runAssistant d a

asIO1 :: (a -> Assistant b) -> Assistant (a -> IO b)
asIO1 a = do
	d <- reader id
	return $ \v -> runAssistant d $ a v

asIO2 :: (a -> b -> Assistant c) -> Assistant (a -> b -> IO c)
asIO2 a = do
	d <- reader id
	return $ \v1 v2 -> runAssistant d (a v1 v2)

{- Runs an IO action on a selected field of the AssistantData. -}
(<<~) :: (a -> IO b) -> (AssistantData -> a) -> Assistant b
io <<~ v = reader v >>= liftIO . io

debug :: [String] -> Assistant ()
debug = logaction debugM

notice :: [String] -> Assistant ()
notice = logaction noticeM

logaction :: (String -> String -> IO ()) -> [String] -> Assistant ()
logaction a ws = do
	ThreadName name <- getAssistant threadName
	liftIO $ a name $ unwords $ (name ++ ":") : ws

{- git-annex assistant monad
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses #-}

module Assistant.Monad (
	Assistant,
	AssistantData(..),
	newAssistantData,
	runAssistant,
	getAssistant,
	withAssistant,
	liftAnnex,
	(<~>),
	(<<~),
	daemonStatus,
	asIO,
	asIO2,
) where

import "mtl" Control.Monad.Reader
import Control.Monad.Base (liftBase, MonadBase)

import Common.Annex
import Assistant.Types.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.Types.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Types.Pushes
import Assistant.Commits
import Assistant.Changes
import Assistant.BranchChange

newtype Assistant a = Assistant { mkAssistant :: ReaderT AssistantData IO a }
	deriving (
		Monad,
		MonadIO,
		MonadReader AssistantData,
		Functor,
		Applicative
	)

instance MonadBase IO Assistant where
	liftBase = Assistant . liftBase

data AssistantData = AssistantData
	{ threadName :: String
	, threadState :: ThreadState
	, daemonStatusHandle :: DaemonStatusHandle
	, scanRemoteMap :: ScanRemoteMap
	, transferQueue :: TransferQueue
	, transferSlots :: TransferSlots
	, pushNotifier :: PushNotifier
	, failedPushMap :: FailedPushMap
	, commitChan :: CommitChan
	, changeChan :: ChangeChan
	, branchChangeHandle :: BranchChangeHandle
	}

newAssistantData :: ThreadState -> DaemonStatusHandle -> IO AssistantData
newAssistantData st dstatus = AssistantData
	<$> pure "main"
	<*> pure st
	<*> pure dstatus
	<*> newScanRemoteMap
	<*> newTransferQueue
	<*> newTransferSlots
	<*> newPushNotifier
	<*> newFailedPushMap
	<*> newCommitChan
	<*> newChangeChan
	<*> newBranchChangeHandle

runAssistant :: Assistant a -> AssistantData -> IO a
runAssistant a = runReaderT (mkAssistant a)

getAssistant :: (AssistantData -> a) -> Assistant a
getAssistant = reader

{- Runs an action in the git-annex monad. Note that the same monad state
 - is shared amoung all assistant threads, so only one of these can run at
 - a time. Therefore, long-duration actions should be avoided. -}
liftAnnex :: Annex a -> Assistant a
liftAnnex a = do
	st <- reader threadState
	liftIO $ runThreadState st a

{- Runs an IO action, passing it an IO action that runs an Assistant action. -}
(<~>) :: (IO a -> IO b) -> Assistant a -> Assistant b
io <~> a = do
	d <- reader id
	liftIO $ io $ runAssistant a d

{- Creates an IO action that will run an Assistant action when run. -}
asIO :: (a -> Assistant b) -> Assistant (a -> IO b)
asIO a = do
	d <- reader id
	return $ \v -> runAssistant (a v) d

{- Creates an IO action that will run an Assistant action when run. -}
asIO2 :: (a -> b -> Assistant c) -> Assistant (a -> b -> IO c)
asIO2 a = do
	d <- reader id
	return $ \v1 v2 -> runAssistant (a v1 v2) d

{- Runs an IO action on a selected field of the AssistantData. -}
(<<~) :: (a -> IO b) -> (AssistantData -> a) -> Assistant b
io <<~ v = reader v >>= liftIO . io

withAssistant :: (AssistantData -> a) -> (a -> IO b) -> Assistant b
withAssistant v io = io <<~ v

daemonStatus :: Assistant DaemonStatus
daemonStatus = getDaemonStatus <<~ daemonStatusHandle

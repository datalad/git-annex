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
	liftAnnex
) where

import "mtl" Control.Monad.Reader
import Control.Monad.Base (liftBase, MonadBase)

import Common.Annex
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Pushes
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
	{ threadState :: ThreadState
	, daemonStatus :: DaemonStatusHandle
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
	<$> pure st
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

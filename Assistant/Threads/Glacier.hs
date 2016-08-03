{- git-annex assistant Amazon Glacier retrieval
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant.Threads.Glacier where

import Assistant.Common
import Utility.ThreadScheduler
import qualified Types.Remote as Remote
import qualified Remote.Glacier as Glacier
import Types.Transfer
import Logs.Transfer
import Assistant.DaemonStatus
import Assistant.TransferQueue

import qualified Data.Set as S

{- Wakes up every half hour and checks if any glacier remotes have failed
 - downloads. If so, runs glacier-cli to check if the files are now
 - available, and queues the downloads. -}
glacierThread :: NamedThread
glacierThread = namedThread "Glacier" $ runEvery (Seconds 3600) <~> go
  where
	isglacier r = Remote.remotetype r == Glacier.remote
	go = do
		rs <- filter isglacier . syncDataRemotes <$> getDaemonStatus
		forM_ rs $ \r -> 
			check r =<< liftAnnex (getFailedTransfers $ Remote.uuid r)
	check _ [] = noop
	check r l = do
		let keys = map getkey l
		(availkeys, failedkeys) <- liftAnnex $ Glacier.jobList r keys
		let s = S.fromList (failedkeys ++ availkeys)
		let l' = filter (\p -> S.member (getkey p) s) l
		forM_ l' $ \(t, info) -> do
			liftAnnex $ removeFailedTransfer t
			queueTransferWhenSmall "object available from glacier" (associatedFile info) t r
	getkey = transferKey . fst

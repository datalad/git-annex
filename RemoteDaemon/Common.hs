{- git-remote-daemon utilities
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module RemoteDaemon.Common
	( liftAnnex
	, inLocalRepo
	, checkShouldFetch
	, ConnectionStatus(..)
	, robustConnection
	) where

import qualified Annex
import Annex.Common
import RemoteDaemon.Types
import qualified Git
import Annex.CatFile
import Utility.ThreadScheduler

import Control.Concurrent

-- Runs an Annex action. Long-running actions should be avoided,
-- since only one liftAnnex can be running at a time, across all
-- transports.
liftAnnex :: TransportHandle -> Annex a -> IO a
liftAnnex (TransportHandle _ stmv rd) a = do
	st <- takeMVar stmv
	(r, (st', _rd)) <- Annex.run (st, rd) a
	putMVar stmv st'
	return r

inLocalRepo :: TransportHandle -> (Git.Repo -> IO a) -> IO a
inLocalRepo (TransportHandle (LocalRepo g) _ _) a = a g

-- Check if some shas should be fetched from the remote,
-- and presumably later merged.
checkShouldFetch :: RemoteGitConfig -> TransportHandle -> [Git.Sha] -> IO Bool
checkShouldFetch gc transporthandle shas
	| remoteAnnexPull gc = checkNewShas transporthandle shas
	| otherwise = return False

-- Check if any of the shas are actally new in the local git repo,
-- to avoid unnecessary fetching.
checkNewShas :: TransportHandle -> [Git.Sha] -> IO Bool
checkNewShas transporthandle = check
  where
	check [] = return True
	check (r:rs) = maybe (check rs) (const $ return False)
		=<< liftAnnex transporthandle (catObjectDetails r)

data ConnectionStatus = ConnectionStopping | ConnectionClosed

{- Make connection robust, retrying on error, with exponential backoff. -}
robustConnection :: Int -> IO ConnectionStatus -> IO ()
robustConnection backoff a = 
	caught =<< a `catchNonAsync` (const $ return ConnectionClosed)
  where
	caught ConnectionStopping = return ()
	caught ConnectionClosed = do
		threadDelaySeconds (Seconds backoff)
		robustConnection increasedbackoff a

	increasedbackoff
		| b2 > maxbackoff = maxbackoff
		| otherwise = b2
	  where
		b2 = backoff * 2
		maxbackoff = 3600 -- one hour

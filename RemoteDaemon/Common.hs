{- git-remote-daemon utilities
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Common
	( liftAnnex
	, inLocalRepo
	, checkNewShas
	) where

import qualified Annex
import Annex.Common
import RemoteDaemon.Types
import qualified Git
import Annex.CatFile

import Control.Concurrent

-- Runs an Annex action. Long-running actions should be avoided,
-- since only one liftAnnex can be running at a time, across all
-- transports.
liftAnnex :: TransportHandle -> Annex a -> IO a
liftAnnex (TransportHandle _ annexstate) a = do
	st <- takeMVar annexstate
	(r, st') <- Annex.run st a
	putMVar annexstate st'
	return r

inLocalRepo :: TransportHandle -> (Git.Repo -> IO a) -> IO a
inLocalRepo (TransportHandle (LocalRepo g) _) a = a g

-- Check if any of the shas are actally new in the local git repo,
-- to avoid unnecessary fetching.
checkNewShas :: TransportHandle -> [Git.Sha] -> IO Bool
checkNewShas transporthandle = check
  where
	check [] = return True
	check (r:rs) = maybe (check rs) (const $ return False)
		=<< liftAnnex transporthandle (catObjectDetails r)

{- git-annex assistant remote checker thread
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.RemoteChecker (
	remoteCheckerThread
) where

import Assistant.Common
import Utility.ThreadScheduler
import Assistant.Types.UrlRenderer
import Assistant.Alert
import Remote
import qualified Types.Remote as Remote
import qualified Git.Fsck
import Assistant.Repair
import qualified Git
import Assistant.RemoteProblem
import Assistant.Sync

import Data.Function

{- Waits for problems with remotes, and tries to fsck the remote and repair
 - the problem. -}
remoteCheckerThread :: UrlRenderer -> NamedThread
remoteCheckerThread urlrenderer = namedThread "RemoteChecker" $ forever $ do
	mapM_ (handleProblem urlrenderer)
		=<< liftIO . filterM (checkAvailable True)
		=<< nubremotes <$> getRemoteProblems
	liftIO $ threadDelaySeconds (Seconds 60)
  where
	nubremotes = nubBy ((==) `on` Remote.uuid)

handleProblem :: UrlRenderer -> Remote -> Assistant ()
handleProblem urlrenderer rmt
	| Git.repoIsLocal r && not (Git.repoIsLocalUnknown r) = do
		fsckresults <- showFscking urlrenderer (Just $ Remote.name rmt) $ tryNonAsync $
			Git.Fsck.findBroken True r
		whenM (repairWhenNecessary urlrenderer (Remote.uuid rmt) (Just rmt) fsckresults) $
			syncRemote rmt
	| otherwise = noop
  where
	r = Remote.repo rmt

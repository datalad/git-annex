{- git-annex assistant config monitor thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.ConfigMonitor where

import Assistant.Common
import Assistant.BranchChange
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Utility.ThreadScheduler
import Logs.UUID
import Logs.Trust
import Logs.Remote
import Logs.PreferredContent
import Logs.Group
import Remote.List (remoteListRefresh)
import qualified Git
import qualified Git.LsTree as LsTree
import qualified Annex.Branch
import qualified Annex

import qualified Data.Set as S

thisThread :: ThreadName
thisThread = "ConfigMonitor"

{- This thread detects when configuration changes have been made to the
 - git-annex branch and reloads cached configuration.
 -
 - If the branch is frequently changing, it's checked for configuration
 - changes no more often than once every 60 seconds. On the other hand,
 - if the branch has not changed in a while, configuration changes will
 - be detected immediately.
 -}
configMonitorThread :: ThreadState -> DaemonStatusHandle -> BranchChangeHandle -> NamedThread
configMonitorThread st dstatus branchhandle = thread $ do
	r <- runThreadState st Annex.gitRepo
	go r =<< getConfigs r
	where
		thread = NamedThread thisThread
		
		go r old = do
			threadDelaySeconds (Seconds 60)
			waitBranchChange branchhandle
			new <- getConfigs r
			when (old /= new) $ do
				let changedconfigs = new `S.difference` old
				debug thisThread $ "reloading config" : 
					map fst (S.toList changedconfigs)
				reloadConfigs st dstatus changedconfigs
			go r new

{- Config files, and their checksums. -}
type Configs = S.Set (FilePath, String)

{- All git-annex's config files, and actions to run when they change. -}
configFilesActions :: [(FilePath, Annex ())]
configFilesActions =
	[ (uuidLog, void $ uuidMapLoad)
	, (remoteLog, void remoteListRefresh)
	, (trustLog, void trustMapLoad)
	, (groupLog, void groupMapLoad)
	-- Preferred content settings depend on most of the other configs,
	-- so will be reloaded whenever any configs change.
	, (preferredContentLog, noop)
	]

reloadConfigs :: ThreadState -> DaemonStatusHandle -> Configs -> IO ()
reloadConfigs st dstatus changedconfigs = runThreadState st $ do
	sequence_ as
	void preferredContentMapLoad
	{- Changes to the remote log, or the trust log, can affect the
	 - syncRemotes list -}
	when (Logs.Remote.remoteLog `elem` fs || Logs.Trust.trustLog `elem` fs)  $
		updateSyncRemotes dstatus
	where
		(fs, as) = unzip $ filter (flip S.member changedfiles . fst)
			configFilesActions
		changedfiles = S.map fst changedconfigs

getConfigs :: Git.Repo -> IO Configs
getConfigs r = S.fromList . map extract
	<$> LsTree.lsTreeFiles Annex.Branch.fullname files r
	where
		files = map fst configFilesActions
		extract treeitem = (LsTree.file treeitem, LsTree.sha treeitem)

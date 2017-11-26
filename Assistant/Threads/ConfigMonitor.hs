{- git-annex assistant config monitor thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.ConfigMonitor where

import Assistant.Common
import Assistant.BranchChange
import Assistant.DaemonStatus
import Assistant.Commits
import Utility.ThreadScheduler
import Logs
import Logs.UUID
import Logs.Trust
import Logs.PreferredContent
import Logs.Group
import Logs.NumCopies
import Remote.List (remoteListRefresh)
import qualified Git.LsTree as LsTree
import Git.Types
import Git.FilePath
import qualified Annex.Branch

import qualified Data.Set as S

{- This thread detects when configuration changes have been made to the
 - git-annex branch and reloads cached configuration.
 -
 - If the branch is frequently changing, it's checked for configuration
 - changes no more often than once every 60 seconds. On the other hand,
 - if the branch has not changed in a while, configuration changes will
 - be detected immediately.
 -}
configMonitorThread :: NamedThread
configMonitorThread = namedThread "ConfigMonitor" $ loop =<< getConfigs
  where
	loop old = do
		waitBranchChange
		new <- getConfigs
		when (old /= new) $ do
			let changedconfigs = new `S.difference` old
			debug $ "reloading config" : 
				map fst (S.toList changedconfigs)
			reloadConfigs new
			{- Record a commit to get this config
			 - change pushed out to remotes. -}
			recordCommit
		liftIO $ threadDelaySeconds (Seconds 60)
		loop new

{- Config files, and their checksums. -}
type Configs = S.Set (FilePath, Sha)

{- All git-annex's config files, and actions to run when they change. -}
configFilesActions :: [(FilePath, Assistant ())]
configFilesActions =
	[ (uuidLog, void $ liftAnnex uuidMapLoad)
	, (remoteLog, void $ liftAnnex remoteListRefresh)
	, (trustLog, void $ liftAnnex trustMapLoad)
	, (groupLog, void $ liftAnnex groupMapLoad)
	, (numcopiesLog, void $ liftAnnex globalNumCopiesLoad)
	, (scheduleLog, void updateScheduleLog)
	-- Preferred and required content settings depend on most of the
	-- other configs, so will be reloaded whenever any configs change.
	, (preferredContentLog, noop)
	, (requiredContentLog, noop)
	, (groupPreferredContentLog, noop)
	]

reloadConfigs :: Configs -> Assistant ()
reloadConfigs changedconfigs = do
	sequence_ as
	void $ liftAnnex preferredRequiredMapsLoad
	{- Changes to the remote log, or the trust log, can affect the
	 - syncRemotes list. Changes to the uuid log may affect its
	 - display so are also included. -}
	when (any (`elem` fs) [remoteLog, trustLog, uuidLog])
		updateSyncRemotes
  where
	(fs, as) = unzip $ filter (flip S.member changedfiles . fst)
		configFilesActions
	changedfiles = S.map fst changedconfigs

getConfigs :: Assistant Configs
getConfigs = S.fromList . map extract
	<$> liftAnnex (inRepo $ LsTree.lsTreeFiles Annex.Branch.fullname files)
  where
	files = map fst configFilesActions
	extract treeitem = (getTopFilePath $ LsTree.file treeitem, LsTree.sha treeitem)

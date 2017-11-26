{- git-annex assistant fscking
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Fsck where

import Assistant.Common
import Types.ScheduledActivity
import qualified Types.Remote as Remote
import Annex.UUID
import Assistant.Alert
import Assistant.Types.UrlRenderer
import Logs.Schedule
import qualified Annex

import qualified Data.Set as S

{- Displays a nudge in the webapp if a fsck is not configured for
 - the specified remote, or for the local repository. -}
fsckNudge :: UrlRenderer -> Maybe Remote -> Assistant ()
fsckNudge urlrenderer mr
	| maybe True fsckableRemote mr =
		whenM (liftAnnex $ annexFsckNudge <$> Annex.getGitConfig) $
			unlessM (liftAnnex $ checkFscked mr) $
				notFsckedNudge urlrenderer mr
	| otherwise = noop

fsckableRemote :: Remote -> Bool
fsckableRemote = isJust . Remote.remoteFsck

{- Checks if the remote, or the local repository, has a fsck scheduled.
 - Only looks at fscks configured to run via the local repository, not
 - other repositories. -}
checkFscked :: Maybe Remote -> Annex Bool
checkFscked mr = any wanted . S.toList <$> (scheduleGet =<< getUUID)
  where
	wanted = case mr of
		Nothing -> isSelfFsck
		Just r -> flip isFsckOf (Remote.uuid r)

isSelfFsck :: ScheduledActivity -> Bool
isSelfFsck (ScheduledSelfFsck _ _) = True
isSelfFsck _ = False

isFsckOf :: ScheduledActivity -> UUID -> Bool
isFsckOf (ScheduledRemoteFsck u _ _) u' = u == u'
isFsckOf _ _ = False

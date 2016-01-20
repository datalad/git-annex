{- Remote state logs.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.RemoteState (
	getRemoteState,
	setRemoteState,
) where

import Annex.Common
import Logs
import Logs.UUIDBased
import qualified Annex.Branch
import qualified Annex

import qualified Data.Map as M
import Data.Time.Clock.POSIX

type RemoteState = String

setRemoteState :: UUID -> Key -> RemoteState -> Annex ()
setRemoteState u k s = do
	ts <- liftIO getPOSIXTime
	config <- Annex.getGitConfig
	Annex.Branch.change (remoteStateLogFile config k) $
		showLogNew id . changeLog ts u s . parseLogNew Just

getRemoteState :: UUID -> Key -> Annex (Maybe RemoteState)
getRemoteState u k = do
	config <- Annex.getGitConfig
	extract . parseLogNew Just
		<$> Annex.Branch.get (remoteStateLogFile config k)
  where
	extract m = value <$> M.lookup u m

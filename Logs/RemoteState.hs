{- Remote state logs.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.RemoteState (
	getRemoteState,
	setRemoteState,
) where

import Common.Annex
import Logs
import Logs.UUIDBased
import qualified Annex.Branch

import qualified Data.Map as M
import Data.Time.Clock.POSIX

type RemoteState = String

setRemoteState :: UUID -> Key -> RemoteState -> Annex ()
setRemoteState u k s = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change (remoteStateLogFile k) $
		showLogNew id . changeLog ts u s . parseLogNew Just

getRemoteState :: UUID -> Key -> Annex (Maybe RemoteState)
getRemoteState u k = extract . parseLogNew Just
	<$> Annex.Branch.get (remoteStateLogFile k)
  where
	extract m = value <$> M.lookup u m

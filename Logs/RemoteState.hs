{- Remote state logs.
 -
 - Copyright 2014, 2019 Joey Hess <id@joeyh.name>
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
import Data.ByteString.Builder

type RemoteState = String

setRemoteState :: UUID -> Key -> RemoteState -> Annex ()
setRemoteState u k s = do
	c <- liftIO currentVectorClock
	config <- Annex.getGitConfig
	Annex.Branch.change (remoteStateLogFile config k) $
		buildRemoteState . changeLog c u s . parseLogNew Just . decodeBL

buildRemoteState :: Log RemoteState -> Builder
buildRemoteState = buildLogNew (byteString . encodeBS)

getRemoteState :: UUID -> Key -> Annex (Maybe RemoteState)
getRemoteState u k = do
	config <- Annex.getGitConfig
	extract . parseLogNew Just . decodeBL
		<$> Annex.Branch.get (remoteStateLogFile config k)
  where
	extract m = value <$> M.lookup u m

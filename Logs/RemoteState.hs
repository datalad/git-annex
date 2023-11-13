{- Remote state logs.
 -
 - Copyright 2014, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.RemoteState (
	getRemoteState,
	setRemoteState,
) where

import Annex.Common
import Types.RemoteState
import Logs
import Logs.UUIDBased
import Logs.MapLog
import qualified Annex.Branch
import qualified Annex

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

type RemoteState = String

setRemoteState :: RemoteStateHandle -> Key -> RemoteState -> Annex ()
setRemoteState (RemoteStateHandle u) k s = do
	c <- currentVectorClock
	config <- Annex.getGitConfig
	Annex.Branch.change
		(Annex.Branch.RegardingUUID [u])
		(remoteStateLogFile config k)
		(buildRemoteState . changeLog c u s . parseRemoteState)

buildRemoteState :: Log RemoteState -> Builder
buildRemoteState = buildLogNew (byteString . encodeBS)

getRemoteState :: RemoteStateHandle -> Key -> Annex (Maybe RemoteState)
getRemoteState (RemoteStateHandle u) k = do
	config <- Annex.getGitConfig
	extract . fromMapLog . parseRemoteState
		<$> Annex.Branch.get (remoteStateLogFile config k)
  where
	extract m = value <$> M.lookup u m

parseRemoteState :: L.ByteString -> Log RemoteState
parseRemoteState = parseLogNew (decodeBS <$> A.takeByteString)

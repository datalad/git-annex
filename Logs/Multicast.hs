{- git-annex multicast fingerprint log
 -
 - Copyright 2017, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Multicast (
	Fingerprint(..),
	recordFingerprint,
	knownFingerPrints,
) where

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.UUIDBased

import qualified Data.Map as M
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

newtype Fingerprint = Fingerprint String
	deriving (Eq, Read, Show)

recordFingerprint :: Fingerprint -> UUID -> Annex ()
recordFingerprint fp uuid = do
	c <- currentVectorClock
	Annex.Branch.change multicastLog $
		buildLogOld buildFindgerPrint
			. changeLog c uuid fp
			. parseLogOld fingerprintParser

knownFingerPrints :: Annex (M.Map UUID Fingerprint)
knownFingerPrints = simpleMap . parseLogOld fingerprintParser
	<$> Annex.Branch.get activityLog

fingerprintParser :: A.Parser Fingerprint
fingerprintParser = maybe (fail "fingerprint parse failed") pure 
	. readish . decodeBS =<< A.takeByteString

buildFindgerPrint :: Fingerprint -> Builder
buildFindgerPrint = byteString . encodeBS . show

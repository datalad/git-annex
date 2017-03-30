{- git-annex multicast fingerprint log
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Multicast (
	Fingerprint(..),
	recordFingerprint,
	knownFingerPrints,
) where

import Data.Time.Clock.POSIX

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.UUIDBased

import qualified Data.Map as M

newtype Fingerprint = Fingerprint String
	deriving (Eq, Read, Show)

recordFingerprint :: Fingerprint -> UUID -> Annex ()
recordFingerprint fp uuid = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change multicastLog $
		showLog show . changeLog ts uuid fp . parseLog readish

knownFingerPrints :: Annex (M.Map UUID Fingerprint)
knownFingerPrints = simpleMap . parseLog readish <$> Annex.Branch.get activityLog

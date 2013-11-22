{- Data type for a distribution of git-annex
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Distribution where

import Types.Key
import Data.Time.Clock
import Git.Config (isTrue, boolConfig)

data GitAnnexDistribution = GitAnnexDistribution
	{ distributionUrl :: String
	, distributionKey :: Key
	, distributionVersion :: GitAnnexVersion
	, distributionReleasedate :: UTCTime
	, distributionUrgentUpgrade :: Maybe GitAnnexVersion
	}
	deriving (Read, Show, Eq)

type GitAnnexVersion = String

data AutoUpgrade = AskUpgrade | AutoUpgrade | NoAutoUpgrade
	deriving (Eq)

toAutoUpgrade :: (Maybe String) -> AutoUpgrade
toAutoUpgrade Nothing = AskUpgrade
toAutoUpgrade (Just s)
	| s == "ask" = AskUpgrade
	| isTrue s == Just True = AutoUpgrade
	| otherwise = NoAutoUpgrade

fromAutoUpgrade :: AutoUpgrade -> String
fromAutoUpgrade AskUpgrade = "ask"
fromAutoUpgrade AutoUpgrade = boolConfig True
fromAutoUpgrade NoAutoUpgrade = boolConfig False

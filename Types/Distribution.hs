{- Data type for a distribution of git-annex
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Distribution where

import Data.Time.Clock

data GitAnnexDistribution = GitAnnexDistribution
	{ distributionUrl :: String
	, distributionSha256 :: String
	, distributionVersion :: GitAnnexVersion
	, distributionReleasedate :: UTCTime
	, distributionUrgentUpgrade :: Maybe GitAnnexVersion
	}
	deriving (Read, Show, Eq)

type GitAnnexVersion = String

{- Data type for a distribution of git-annex
 -
 - Copyright 2013, 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Distribution where

import Utility.PartialPrelude
import Types.Key
import Key
import Data.Time.Clock
import Git.Config (isTrue, boolConfig)

import Data.String.Utils

type GitAnnexVersion = String

data GitAnnexDistribution = GitAnnexDistribution
	{ distributionUrl :: String
	, distributionKey :: Key
	, distributionVersion :: GitAnnexVersion
	, distributionReleasedate :: UTCTime
	, distributionUrgentUpgrade :: Maybe GitAnnexVersion
	}
	deriving (Read, Show, Eq)

{- The first line of the info file is in the format old versions of
 - git-annex expect to read a GitAnnexDistribution.
 - The remainder of the file is in the new format.
 - This works because old versions of git-annex used readish to parse
 - the file, and that ignores the second line.
 -}
formatInfoFile :: GitAnnexDistribution -> String
formatInfoFile d = replace "keyVariant = " "keyBackendName = " (show d) ++
	"\n" ++ formatGitAnnexDistribution d

parseInfoFile :: String -> Maybe GitAnnexDistribution
parseInfoFile s = case lines s of
	(_oldformat:rest) -> parseGitAnnexDistribution (unlines rest)
	_ -> Nothing

formatGitAnnexDistribution :: GitAnnexDistribution -> String
formatGitAnnexDistribution d = unlines
	[ distributionUrl d
	, key2file (distributionKey d)
	, distributionVersion d
	, show (distributionReleasedate d)
	, maybe "" show (distributionUrgentUpgrade d)
	]

parseGitAnnexDistribution :: String -> Maybe GitAnnexDistribution
parseGitAnnexDistribution s = case lines s of
	(u:k:v:d:uu:_) -> GitAnnexDistribution
		<$> pure u
		<*> file2key k
		<*> pure v
		<*> readish d
		<*> pure (readish uu)
	_ -> Nothing

data AutoUpgrade = AskUpgrade | AutoUpgrade | NoAutoUpgrade
	deriving (Eq)

toAutoUpgrade :: Maybe String -> AutoUpgrade
toAutoUpgrade Nothing = AskUpgrade
toAutoUpgrade (Just s)
	| s == "ask" = AskUpgrade
	| isTrue s == Just True = AutoUpgrade
	| otherwise = NoAutoUpgrade

fromAutoUpgrade :: AutoUpgrade -> String
fromAutoUpgrade AskUpgrade = "ask"
fromAutoUpgrade AutoUpgrade = boolConfig True
fromAutoUpgrade NoAutoUpgrade = boolConfig False

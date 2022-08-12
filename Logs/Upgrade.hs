{- git-annex upgrade log
 -
 - This file is stored locally in .git/annex/, not in the git-annex branch.
 -
 - The format: "version timestamp"
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Upgrade (
	writeUpgradeLog,
	readUpgradeLog,
	timeOfUpgrade,
) where

import Annex.Common
import Utility.TimeStamp
import Logs.File
import Types.RepoVersion

import Data.Time.Clock.POSIX

writeUpgradeLog :: RepoVersion -> POSIXTime-> Annex ()
writeUpgradeLog v t = do
	logf <- fromRepo gitAnnexUpgradeLog
	lckf <- fromRepo gitAnnexUpgradeLock
	appendLogFile logf lckf $ encodeBL $
		show (fromRepoVersion v) ++ " " ++ show t

readUpgradeLog :: Annex [(RepoVersion, POSIXTime)]
readUpgradeLog = do
	logfile <- fromRawFilePath <$> fromRepo gitAnnexUpgradeLog
	ifM (liftIO $ doesFileExist logfile)
		( mapMaybe parse . lines
			<$> liftIO (readFileStrict logfile)
		, return []
		)
  where
	parse line = case (readish sint, parsePOSIXTime ts) of
		(Just v, Just t) -> Just (RepoVersion v, t)
		_ -> Nothing
	  where
		(sint, ts) = separate (== ' ') line

timeOfUpgrade :: RepoVersion -> Annex (Maybe POSIXTime)
timeOfUpgrade want = do
	l <- readUpgradeLog
	return $ case filter (\(v, _) -> v == want) l of
		[] -> Nothing
		l' -> Just (minimum (map snd l'))


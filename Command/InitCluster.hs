{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.InitCluster where

import Command
import qualified Annex
import Types.Cluster
import Logs.UUID
import Config
import Annex.UUID
import Git.Remote (isLegalName)

import qualified Data.Map as M

cmd :: Command
cmd = command "initcluster" SectionSetup "initialize a new cluster"
	(paramPair paramName paramDesc) (withParams seek)

seek :: CmdParams -> CommandSeek
seek (clustername:desc:[]) = commandAction $
	start clustername (toUUIDDesc desc)
seek (clustername:[]) = commandAction $
	start clustername $ toUUIDDesc ("cluster " ++ clustername)
seek _ = giveup "Expected two parameters, name and description."

start :: RemoteName -> UUIDDesc -> CommandStart
start clustername desc = starting "initcluster" ai si $ do
	unless (isLegalName clustername) $
		giveup "That cluster name is not a valid git remote name."

	myclusters <- annexClusters <$> Annex.getGitConfig
	unless (M.member clustername myclusters) $ do
		cu <- fromMaybe (giveup "unable to generate a cluster UUID") 
			<$> genClusterUUID <$> liftIO genUUID
		setConfig (annexConfig ("cluster." <> encodeBS clustername))
			(fromUUID (fromClusterUUID cu))
		describeUUID (fromClusterUUID cu) desc

	next $ return True
  where
	ai = ActionItemOther (Just (UnquotedString clustername))
	si = SeekInput [clustername]

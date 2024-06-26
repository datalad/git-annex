{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.ExtendCluster where

import Command
import qualified Annex
import Types.Cluster
import Config
import qualified Remote

import qualified Data.Map as M

cmd :: Command
cmd = command "extendcluster" SectionSetup "add an gateway to a cluster"
	(paramPair paramRemote paramName) (withParams seek)

seek :: CmdParams -> CommandSeek
seek (remotename:clustername:[]) = Remote.byName (Just clusterremotename) >>= \case
	Just clusterremote -> 
		case mkClusterUUID (Remote.uuid clusterremote) of
			Just cu -> commandAction $ start cu clustername
			Nothing -> giveup $ clusterremotename 
				++ " is not a cluster remote."
	Nothing -> giveup $ "Expected to find a cluster remote named " 
		++ clusterremotename
		++ " that is accessed via " ++ remotename
		++ ", but there is no such remote. Perhaps you need to"
		++ "git fetch from " ++ remotename 
		++ ", or git-annex updatecluster needs to be run there?"
  where
	clusterremotename = remotename ++ "-" ++ clustername
seek _ = giveup "Expected two parameters, gateway and clustername."

start :: ClusterUUID -> String -> CommandStart
start cu clustername = starting "extendcluster" ai si $ do
	myclusters <- annexClusters <$> Annex.getGitConfig
	unless (M.member clustername myclusters) $ do
		setConfig (annexConfig ("cluster." <> encodeBS clustername))
			(fromUUID (fromClusterUUID cu))
	next $ return True
  where
	ai = ActionItemOther (Just (UnquotedString clustername))
	si = SeekInput [clustername]

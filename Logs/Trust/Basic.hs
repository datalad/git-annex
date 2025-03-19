{- git-annex trust log, basics
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Trust.Basic (
	module X,
	trustSet,
	trustMapRaw,
	trustMap',
	trustMapLoad',
) where

import Annex.Common
import Types.TrustLevel
import qualified Annex.Branch
import qualified Annex
import qualified Types.Remote
import Logs
import Logs.UUIDBased
import Logs.Trust.Pure as X

import qualified Data.Map as M

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid@(UUID _) level = do
	c <- currentVectorClock
	Annex.Branch.change (Annex.Branch.RegardingUUID [uuid]) trustLog $
		buildLogOld buildTrustLevel .
			changeLog c uuid level .
				parseLogOld trustLevelParser
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
trustSet NoUUID _ = error "unknown UUID; cannot modify"

{- Does not include forcetrust or git config values, just those from the
 - log file. -}
trustMapRaw :: Annex TrustMap
trustMapRaw = calcTrustMap <$> Annex.Branch.get trustLog

trustMap' :: [Remote] -> Annex TrustMap
trustMap' l = maybe (trustMapLoad' l) return =<< Annex.getState Annex.trustmap

trustMapLoad' :: [Remote] -> Annex TrustMap
trustMapLoad' l = do
	forceoverrides <- Annex.getState Annex.forcetrust
	let untrustoverrides = M.fromList $
		map (\r -> (Types.Remote.uuid r, UnTrusted))
		(filter Types.Remote.untrustworthy l)
	logged <- trustMapRaw
	let configured = M.fromList $ mapMaybe configuredtrust l
	let m = M.unionWith min untrustoverrides $
		M.union forceoverrides $
		M.union configured logged
	Annex.changeState $ \s -> s { Annex.trustmap = Just m }
	return m
  where
	configuredtrust r = (\lvl -> Just (Types.Remote.uuid r, lvl))
		=<< readTrustLevel 
		=<< remoteAnnexTrustLevel (Types.Remote.gitconfig r)

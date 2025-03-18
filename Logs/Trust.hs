{- git-annex trust log
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Trust (
	module X,
	trustLog,
	TrustLevel(..),
	trustGet,
	trustMap,
	trustPartition,
	trustExclude,
	lookupTrust,
	lookupTrust',
	trustMapLoad,
) where

import Annex.Common
import Types.TrustLevel
import qualified Annex
import Logs
import Remote.List
import Logs.Trust.Basic as X

import qualified Data.Map as M

{- Returns a list of UUIDs that the trustLog indicates have the
 - specified trust level.
 - Note that the list can be incomplete for SemiTrusted, since that's
 - the default. -}
trustGet :: TrustLevel -> Annex [UUID]
trustGet level = M.keys . M.filter (== level) <$> trustMap

{- Returns the TrustLevel of a given repo UUID. -}
lookupTrust :: UUID -> Annex TrustLevel
lookupTrust u = lookupTrust' u <$> trustMap

lookupTrust' :: UUID -> TrustMap -> TrustLevel
lookupTrust' u m = fromMaybe def $ M.lookup u m

{- Partitions a list of UUIDs to those matching a TrustLevel and not. -}
trustPartition :: TrustLevel -> [UUID] -> Annex ([UUID], [UUID])
trustPartition level ls
	| level == SemiTrusted = do
		t <- trustGet Trusted
		u <- trustGet UnTrusted
		d <- trustGet DeadTrusted
		let uncandidates = t ++ u ++ d
		return $ partition (`notElem` uncandidates) ls
	| otherwise = do
		candidates <- trustGet level
		return $ partition (`elem` candidates) ls

{- Filters UUIDs to those not matching a TrustLevel. -}
trustExclude :: TrustLevel -> [UUID] -> Annex [UUID]
trustExclude level ls = snd <$> trustPartition level ls

{- trustLog in a map, overridden with any values from forcetrust or
 - the git config. The map is cached for speed. -}
trustMap :: Annex TrustMap
trustMap = maybe trustMapLoad return =<< Annex.getState Annex.trustmap

{- Loads the map, updating the cache, -}
trustMapLoad :: Annex TrustMap
trustMapLoad = trustMapLoad' =<< remoteList

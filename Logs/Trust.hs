{- git-annex trust log
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Trust (
	trustLog,
	TrustLevel(..),
	trustGet,
	trustMap,
	trustSet,
	trustPartition,
	trustExclude,
	lookupTrust,
	trustMapLoad,
	trustMapRaw,
	
	prop_parse_show_TrustLog,
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import Types.TrustLevel
import qualified Annex.Branch
import qualified Annex
import Logs.UUIDBased
import Remote.List
import qualified Types.Remote

{- Filename of trust.log. -}
trustLog :: FilePath
trustLog = "trust.log"

{- Returns a list of UUIDs that the trustLog indicates have the
 - specified trust level.
 - Note that the list can be incomplete for SemiTrusted, since that's
 - the default. -}
trustGet :: TrustLevel -> Annex [UUID]
trustGet level = M.keys . M.filter (== level) <$> trustMap

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid@(UUID _) level = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change trustLog $
		showLog showTrustLog .
			changeLog ts uuid level .
				parseLog (Just . parseTrustLog)
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
trustSet NoUUID _ = error "unknown UUID; cannot modify"

{- Returns the TrustLevel of a given repo UUID. -}
lookupTrust :: UUID -> Annex TrustLevel
lookupTrust u = (fromMaybe SemiTrusted . M.lookup u) <$> trustMap

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
trustMapLoad = do
	overrides <- Annex.getState Annex.forcetrust
	logged <- trustMapRaw
	configured <- M.fromList . catMaybes
		<$> (map configuredtrust <$> remoteList)
	let m = M.union overrides $ M.union configured logged
	Annex.changeState $ \s -> s { Annex.trustmap = Just m }
	return m
  where
	configuredtrust r = (\l -> Just (Types.Remote.uuid r, l))
		=<< readTrustLevel 
		=<< remoteAnnexTrustLevel (Types.Remote.gitconfig r)

{- Does not include forcetrust or git config values, just those from the
 - log file. -}
trustMapRaw :: Annex TrustMap
trustMapRaw = simpleMap . parseLog (Just . parseTrustLog)
	<$> Annex.Branch.get trustLog

{- The trust.log used to only list trusted repos, without a field for the
 - trust status, which is why this defaults to Trusted. -}
parseTrustLog :: String -> TrustLevel
parseTrustLog s = maybe Trusted parse $ headMaybe $ words s
  where
	parse "1" = Trusted
	parse "0" = UnTrusted
	parse "X" = DeadTrusted
	parse _ = SemiTrusted

showTrustLog :: TrustLevel -> String
showTrustLog Trusted = "1"
showTrustLog UnTrusted = "0"
showTrustLog DeadTrusted = "X"
showTrustLog SemiTrusted = "?"

prop_parse_show_TrustLog :: Bool
prop_parse_show_TrustLog = all check [minBound .. maxBound]
  where
	check l = parseTrustLog (showTrustLog l) == l

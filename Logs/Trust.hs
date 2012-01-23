{- git-annex trust log
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Trust (
	TrustLevel(..),
	trustGet,
	trustSet,
	trustPartition,
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import Types.TrustLevel
import qualified Annex.Branch
import qualified Annex
import Logs.UUIDBased
import Remote.List
import Config
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
		showLog showTrust . changeLog ts uuid level . parseLog (Just . parseTrust)
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
trustSet NoUUID _ = error "unknown UUID; cannot modify trust level"

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

{- Read the trustLog into a map, overriding with any
 - values from forcetrust or the git config. The map is cached for speed. -}
trustMap :: Annex TrustMap
trustMap = do
	cached <- Annex.getState Annex.trustmap
	case cached of
		Just m -> return m
		Nothing -> do
			overrides <- Annex.getState Annex.forcetrust
			logged <- simpleMap . parseLog (Just . parseTrust) <$>
				Annex.Branch.get trustLog
			configured <- M.fromList . catMaybes <$>
				(mapM configuredtrust =<< remoteList)
			let m = M.union overrides $ M.union configured logged
			Annex.changeState $ \s -> s { Annex.trustmap = Just m }
			return m
	where
		configuredtrust r =
			maybe Nothing (\l -> Just (Types.Remote.uuid r, l)) <$>
				maybe Nothing convert <$>
					getTrustLevel (Types.Remote.repo r)
		convert "trusted" = Just Trusted
		convert "untrusted" = Just UnTrusted
		convert "semitrusted" = Just SemiTrusted
		convert _ = Nothing

{- The trust.log used to only list trusted repos, without a field for the
 - trust status, which is why this defaults to Trusted. -}
parseTrust :: String -> TrustLevel
parseTrust s = maybe Trusted parse $ headMaybe $ words s
	where
		parse "1" = Trusted
		parse "0" = UnTrusted
		parse "X" = DeadTrusted
		parse _ = SemiTrusted

showTrust :: TrustLevel -> String
showTrust Trusted = "1"
showTrust UnTrusted = "0"
showTrust DeadTrusted = "X"
showTrust SemiTrusted = "?"

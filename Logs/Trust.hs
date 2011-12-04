{- git-annex trust
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Trust (
	TrustLevel(..),
	trustGet,
	trustSet,
	trustPartition
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import Types.TrustLevel
import qualified Annex.Branch
import qualified Annex
import Logs.UUIDBased

{- Filename of trust.log. -}
trustLog :: FilePath
trustLog = "trust.log"

{- Returns a list of UUIDs that the trustLog indicates have the
 - specified trust level.
 - Note that the list can be incomplete for SemiTrusted, since that's
 - the default. -}
trustGet :: TrustLevel -> Annex [UUID]
trustGet level = M.keys . M.filter (== level) <$> trustMap

{- Partitions a list of UUIDs to those matching a TrustLevel and not. -}
trustPartition :: TrustLevel -> [UUID] -> Annex ([UUID], [UUID])
trustPartition level ls
	| level == SemiTrusted = do
		t <- trustGet Trusted
		u <- trustGet UnTrusted
		let uncandidates = t ++ u
		return $ partition (`notElem` uncandidates) ls
	| otherwise = do
		candidates <- trustGet level
		return $ partition (`elem` candidates) ls

{- Read the trustLog into a map, overriding with any
 - values from forcetrust. The map is cached for speed. -}
trustMap :: Annex TrustMap
trustMap = do
	cached <- Annex.getState Annex.trustmap
	case cached of
		Just m -> return m
		Nothing -> do
			overrides <- M.fromList <$> Annex.getState Annex.forcetrust
			m <- (M.union overrides . simpleMap . parseLog parseTrust) <$>
				Annex.Branch.get trustLog
			Annex.changeState $ \s -> s { Annex.trustmap = Just m }
			return m

parseTrust :: String -> Maybe TrustLevel
parseTrust s
	| length w > 0 = Just $ parse $ head w
	-- back-compat; the trust.log used to only list trusted repos
	| otherwise = Just Trusted
	where
		w = words s
		parse "1" = Trusted
		parse "0" = UnTrusted
		parse _ = SemiTrusted

showTrust :: TrustLevel -> String
showTrust SemiTrusted = "?"
showTrust UnTrusted = "0"
showTrust Trusted = "1"

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid@(UUID _) level = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change trustLog $
		showLog showTrust . changeLog ts uuid level . parseLog parseTrust
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
trustSet NoUUID _ = error "unknown UUID; cannot modify trust level"

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

{- Returns a list of UUIDs at the specified trust level. -}
trustGet :: TrustLevel -> Annex [UUID]
trustGet level = M.keys . M.filter (== level) <$> trustMap

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
	| length w > 0 = readMaybe $ head w
	-- back-compat; the trust.log used to only list trusted repos
	| otherwise = Just Trusted
	where
		w = words s

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid level = do
	when (null uuid) $
		error "unknown UUID; cannot modify trust level"
	ts <- liftIO $ getPOSIXTime
	Annex.Branch.change trustLog $
		showLog show . changeLog ts uuid level . parseLog parseTrust
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }

{- Partitions a list of UUIDs to those matching a TrustLevel and not. -}
trustPartition :: TrustLevel -> [UUID] -> Annex ([UUID], [UUID])
trustPartition level ls = do
	candidates <- trustGet level
	return $ partition (`elem` candidates) ls

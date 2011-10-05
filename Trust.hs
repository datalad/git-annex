{- git-annex trust
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Trust (
	TrustLevel(..),
	trustLog,
	trustGet,
	trustSet,
	trustPartition
) where

import qualified Data.Map as M

import Common.Annex
import Types.TrustLevel
import qualified Annex.Branch
import UUID
import qualified Annex

{- Filename of trust.log. -}
trustLog :: FilePath
trustLog = "trust.log"

{- Returns a list of UUIDs at the specified trust level. -}
trustGet :: TrustLevel -> Annex [UUID]
trustGet level = do
	m <- trustMap
	return $ M.keys $ M.filter (== level) m

{- Read the trustLog into a map, overriding with any
 - values from forcetrust. The map is cached for speed. -}
trustMap :: Annex TrustMap
trustMap = do
	cached <- Annex.getState Annex.trustmap
	case cached of
		Just m -> return m
		Nothing -> do
			overrides <- Annex.getState Annex.forcetrust
			l <- Annex.Branch.get trustLog
			let m = M.fromList $ trustMapParse l ++ overrides
			Annex.changeState $ \s -> s { Annex.trustmap = Just m }
			return m

{- Trust map parser. -}
trustMapParse :: String -> [(UUID, TrustLevel)]
trustMapParse s = map pair $ filter (not . null) $ lines s
	where
		pair l
			| length w > 1 = (w !! 0, read (w !! 1) :: TrustLevel)
			-- for back-compat; the trust log used to only
			-- list trusted uuids
			| otherwise = (w !! 0, Trusted)
			where
				w = words l

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid level = do
	when (null uuid) $
		error "unknown UUID; cannot modify trust level"
	Annex.Branch.change trustLog $
		serialize . M.insert uuid level . M.fromList . trustMapParse
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
        where
                serialize m = unlines $ map showpair $ M.toList m
		showpair (u, t) = u ++ " " ++ show t

{- Partitions a list of UUIDs to those matching a TrustLevel and not. -}
trustPartition :: TrustLevel -> [UUID] -> Annex ([UUID], [UUID])
trustPartition level ls = do
	candidates <- trustGet level
	return $ partition (`elem` candidates) ls

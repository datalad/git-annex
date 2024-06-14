{- git-annex cluster types
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, OverloadedStrings #-}

module Types.Cluster (
	ClusterUUID,
	mkClusterUUID,
	isClusterUUID,
	fromClusterUUID,
	ClusterNodeUUID(..),
	Clusters(..),
) where

import Types.UUID

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Char

-- The UUID of a cluster as a whole.
--
-- Cluster UUIDs are specially constructed so that regular repository UUIDs
-- can never be used as a cluster UUID. This is ncessary for security.
-- They are a version 8 UUID with the first octet set to 'a' and the second
-- to 'c'.
newtype ClusterUUID = ClusterUUID UUID
	deriving (Show, Eq, Ord)

-- Smart constructor for a ClusterUUID.
--
-- The input UUID can be any regular UUID (eg V4). It is converted to a valid
-- cluster UUID.
mkClusterUUID :: UUID -> Maybe ClusterUUID
mkClusterUUID (UUID b)
	| B.length b > 14 = Just $ ClusterUUID $ UUID $
		"ac" <> B.drop 2 (B.take 14 b) <> "8" <> B.drop 15 b
	| otherwise = Nothing
mkClusterUUID NoUUID = Nothing

isClusterUUID :: UUID -> Bool
isClusterUUID (UUID b) 
	| B.take 2 b == "ac" = 
#if MIN_VERSION_bytestring(0,11,0)
		B.indexMaybe b 14 == Just eight
#else
		B.length b > 14 && B.head (B.drop 14 b) == eight
#endif
  where
	eight = fromIntegral (ord '8')
isClusterUUID _ = False

fromClusterUUID :: ClusterUUID -> UUID
fromClusterUUID (ClusterUUID u) = u

-- The UUID of a node in a cluster. The UUID can be either the UUID of a
-- repository, or of another cluster. 
newtype ClusterNodeUUID = ClusterNodeUUID { fromClusterNodeUUID :: UUID }
	deriving (Show, Eq, Ord)

-- The same information is indexed two ways to allow fast lookups in either
-- direction.
data Clusters = Clusters
	{ clusterUUIDs :: M.Map ClusterUUID (S.Set ClusterNodeUUID)
	, clusterNodeUUIDs :: M.Map ClusterNodeUUID (S.Set ClusterUUID)
	}

{- git-annex cluster types
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Cluster where

import Types.UUID

import qualified Data.Set as S
import qualified Data.Map as M

-- The UUID of a cluster as a whole.
newtype ClusterUUID = ClusterUUID { fromClusterUUID :: UUID }
	deriving (Show, Eq, Ord)

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

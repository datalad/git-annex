{- git-annex assistant webapp RepoId type
 -
 - Copyright 2012,2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.WebApp.RepoId where

import Annex.Common
import Git.Types (RemoteName)
import qualified Remote

{- Parts of the webapp need to be able to act on repositories that may or
 - may not have a UUID. -}
data RepoId
	= RepoUUID UUID
	| RepoName RemoteName
	deriving (Eq, Ord, Show, Read)

mkRepoId :: Remote -> RepoId
mkRepoId r = case Remote.uuid r of
	NoUUID -> RepoName (Remote.name r)
	u -> RepoUUID u


describeRepoId :: RepoId -> Annex String
describeRepoId (RepoUUID u) = Remote.prettyUUID u
describeRepoId (RepoName n) = return n

repoIdRemote :: RepoId -> Annex (Maybe Remote)
repoIdRemote (RepoUUID u) = Remote.remoteFromUUID u
repoIdRemote (RepoName n) = Remote.byNameOnly n

lacksUUID :: RepoId -> Bool
lacksUUID r = asUUID r == NoUUID

asUUID :: RepoId -> UUID
asUUID (RepoUUID u) = u
asUUID _ = NoUUID

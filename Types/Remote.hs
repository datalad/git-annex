{- git-annex remotes types
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Remote where

import Data.Map as M
import Data.Ord

import qualified Git
import Types.Key
import Types.UUID

type RemoteConfig = M.Map String String

{- There are different types of remotes. -}
data RemoteTypeA a = RemoteType {
	-- human visible type name
	typename :: String,
	-- enumerates remotes of this type
	enumerate :: a [Git.Repo],
	-- generates a remote of this type
	generate :: Git.Repo -> UUID -> Maybe RemoteConfig -> a (RemoteA a),
	-- initializes or changes a remote
	setup :: UUID -> RemoteConfig -> a RemoteConfig
}

instance Eq (RemoteTypeA a) where
	x == y = typename x == typename y

{- An individual remote. -}
data RemoteA a = Remote {
	-- each Remote has a unique uuid
	uuid :: UUID,
	-- each Remote has a human visible name
	name :: String,
	-- Remotes have a use cost; higher is more expensive
	cost :: Int,
	-- Transfers a key to the remote.
	storeKey :: Key -> a Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> a Bool,
	-- retrieves a key's contents to a tmp file, if it can be done cheaply
	retrieveKeyFileCheap :: Key -> FilePath -> a Bool,
	-- removes a key's contents
	removeKey :: Key -> a Bool,
	-- Checks if a key is present in the remote; if the remote
	-- cannot be accessed returns a Left error message.
	hasKey :: Key -> a (Either String Bool),
	-- Some remotes can check hasKey without an expensive network
	-- operation.
	hasKeyCheap :: Bool,
	-- Some remotes can provide additional details for whereis.
	whereisKey :: Maybe (Key -> a [String]),
	-- a Remote can have a persistent configuration store
	config :: Maybe RemoteConfig,
	-- git configuration for the remote
	repo :: Git.Repo,
	-- the type of the remote
	remotetype :: RemoteTypeA a
}

instance Show (RemoteA a) where
	show remote = "Remote { name =\"" ++ name remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (RemoteA a) where
	x == y = uuid x == uuid y

-- order remotes by cost
instance Ord (RemoteA a) where
	compare = comparing cost

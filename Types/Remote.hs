{- git-annex remotes types
 -
 - Most things should not need this, using Remote instead
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Remote where

import Control.Exception
import Data.Map as M
import Data.Ord

import qualified Git
import Types.Key

type RemoteConfig = M.Map String String

{- There are different types of remotes. -}
data RemoteType a = RemoteType {
	-- human visible type name
	typename :: String,
	-- enumerates remotes of this type
	enumerate :: a [Git.Repo],
	-- generates a remote of this type
	generate :: Git.Repo -> String -> Maybe RemoteConfig -> a (Remote a),
	-- initializes or changes a remote
	setup :: String -> RemoteConfig -> a RemoteConfig
}

{- An individual remote. -}
data Remote a = Remote {
	-- each Remote has a unique uuid
	uuid :: String,
	-- each Remote has a human visible name
	name :: String,
	-- Remotes have a use cost; higher is more expensive
	cost :: Int,
	-- Transfers a key to the remote.
	storeKey :: Key -> a Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> a Bool,
	-- removes a key's contents
	removeKey :: Key -> a Bool,
	-- Checks if a key is present in the remote; if the remote
	-- cannot be accessed returns a Left error.
	hasKey :: Key -> a (Either IOException Bool),
	-- Some remotes can check hasKey without an expensive network
	-- operation.
	hasKeyCheap :: Bool,
	-- a Remote can have a persistent configuration store
	config :: Maybe RemoteConfig
}

instance Show (Remote a) where
	show remote = "Remote { name =\"" ++ name remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (Remote a) where
	x == y = uuid x == uuid y

-- order remotes by cost
instance Ord (Remote a) where
	compare = comparing cost

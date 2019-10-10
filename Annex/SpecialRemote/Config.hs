{- git-annex special remote configuration
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.SpecialRemote.Config where

import Common
import Types.Remote (RemoteConfigKey, RemoteConfig)

import qualified Data.Map as M

{- The name of a configured remote is stored in its config using this key. -}
nameKey :: RemoteConfigKey
nameKey = "name"

{- The name of a sameas remote is stored using this key instead. 
 - This prevents old versions of git-annex getting confused. -}
sameasNameKey :: RemoteConfigKey
sameasNameKey = "sameas-name"

lookupName :: RemoteConfig -> Maybe String
lookupName c = M.lookup nameKey c <|> M.lookup sameasNameKey c

{- The uuid that a sameas remote is the same as is stored in this key. -}
sameasUUIDKey :: RemoteConfigKey
sameasUUIDKey = "sameas-uuid"

{- The type of a remote is stored in its config using this key. -}
typeKey :: RemoteConfigKey
typeKey = "type"

autoEnableKey :: RemoteConfigKey
autoEnableKey = "autoenable"


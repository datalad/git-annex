{- git-annex special remote configuration
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.SpecialRemote.Config where

import Common
import Types.Remote (RemoteConfigField, RemoteConfig)

import qualified Data.Map as M

{- The name of a configured remote is stored in its config using this key. -}
nameField :: RemoteConfigField
nameField = "name"

{- The name of a sameas remote is stored using this key instead. 
 - This prevents old versions of git-annex getting confused. -}
sameasNameField :: RemoteConfigField
sameasNameField = "sameas-name"

lookupName :: RemoteConfig -> Maybe String
lookupName c = M.lookup nameField c <|> M.lookup sameasNameField c

{- The uuid that a sameas remote is the same as is stored in this key. -}
sameasUUIDField :: RemoteConfigField
sameasUUIDField = "sameas-uuid"

{- The type of a remote is stored in its config using this key. -}
typeField :: RemoteConfigField
typeField = "type"

autoEnableField :: RemoteConfigField
autoEnableField = "autoenable"


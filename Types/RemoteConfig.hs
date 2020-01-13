{- git-annex remote config types
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE GADTs #-}

module Types.RemoteConfig where

import qualified Data.Map as M
import Data.Typeable

import Types.ProposedAccepted

type RemoteConfigField = ProposedAccepted String

{- What the user provides to configure the remote, and what is stored for
 - later; a bunch of fields and values. -}
type RemoteConfig = M.Map RemoteConfigField (ProposedAccepted String)

{- Before being used a RemoteConfig has to be parsed. -}
type ParsedRemoteConfig = M.Map RemoteConfigField RemoteConfigValue

{- Remotes can have configuration values of many types, so use Typeable
 - to let them all be stored in here. -}
data RemoteConfigValue where
	RemoteConfigValue :: Typeable v => v -> RemoteConfigValue

{- Extracts the value, if the field was parsed to the requested type. -}
getRemoteConfigValue :: Typeable v => RemoteConfigField -> ParsedRemoteConfig -> Maybe v
getRemoteConfigValue f m = case M.lookup f m of
	Just (RemoteConfigValue v) -> cast v
	Nothing -> Nothing

{- Parse a field's value provided by the user into a RemoteConfigValue.
 -
 - The RemoteConfig is provided to the parser function for cases
 - where multiple fields need to be looked at. However, it's important
 - that, when a parser looks at an additional field in that way, the
 - parser list contains a dedicated parser for that field as well.
 - Presence of fields that are not included in this list will cause
 - a parse failure.
 -}
type RemoteConfigParser = (RemoteConfigField, Maybe (ProposedAccepted String) -> RemoteConfig -> Either String RemoteConfigValue)

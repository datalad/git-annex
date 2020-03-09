{- git-annex remote config types
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE GADTs #-}

module Types.RemoteConfig where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable

import Types.ProposedAccepted

type RemoteConfigField = ProposedAccepted String

{- What the user provides to configure the remote, and what is stored for
 - later; a bunch of fields and values. -}
type RemoteConfig = M.Map RemoteConfigField (ProposedAccepted String)

{- Before being used a RemoteConfig has to be parsed. -}
data ParsedRemoteConfig = ParsedRemoteConfig
	{ parsedRemoteConfigMap :: M.Map RemoteConfigField RemoteConfigValue
	, unparsedRemoteConfig :: RemoteConfig
	}

{- Remotes can have configuration values of many types, so use Typeable
 - to let them all be stored in here. -}
data RemoteConfigValue where
	RemoteConfigValue :: Typeable v => v -> RemoteConfigValue

{- Parse a field's value provided by the user into a RemoteConfigValue.
 -
 - The RemoteConfig is provided to the parser function for cases
 - where multiple fields need to be looked at. However, it's important
 - that, when a parser looks at an additional field in that way, the
 - parser list contains a dedicated parser for that field as well.
 - Presence of fields that are not included in this list will cause
 - a parse failure.
 -}
data RemoteConfigFieldParser = RemoteConfigFieldParser
	{ parserForField :: RemoteConfigField
	, valueParser :: Maybe (ProposedAccepted String) -> RemoteConfig -> Either String (Maybe RemoteConfigValue)
	, fieldDesc :: FieldDesc
	, valueDesc :: Maybe ValueDesc
	}

data FieldDesc
	= FieldDesc String
	| HiddenField

newtype ValueDesc = ValueDesc String

data RemoteConfigParser = RemoteConfigParser
	{ remoteConfigFieldParsers :: [RemoteConfigFieldParser]
	, remoteConfigRestPassthrough :: Maybe (RemoteConfigField -> Bool, [(String, FieldDesc)])
	}

mkRemoteConfigParser :: Monad m => [RemoteConfigFieldParser] -> RemoteConfig -> m RemoteConfigParser
mkRemoteConfigParser l _ = pure (RemoteConfigParser l Nothing)

addRemoteConfigParser :: [RemoteConfigFieldParser] -> RemoteConfigParser -> RemoteConfigParser
addRemoteConfigParser l rpc = rpc
	{ remoteConfigFieldParsers = 
		remoteConfigFieldParsers rpc ++ filter isnew l
	}
  where
	s = S.fromList (map parserForField (remoteConfigFieldParsers rpc))
	isnew p = not (S.member (parserForField p) s)

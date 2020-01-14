{- git-annex special remote configuration
 -
 - Copyright 2019-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Annex.SpecialRemote.Config where

import Common
import Types.Remote (RemoteConfigField, RemoteConfig)
import Types.UUID
import Types.ProposedAccepted
import Types.RemoteConfig
import Config
import qualified Git.Config

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable
import GHC.Stack

newtype Sameas t = Sameas t
	deriving (Show)

newtype ConfigFrom t = ConfigFrom t
	deriving (Show)

{- The name of a configured remote is stored in its config using this key. -}
nameField :: RemoteConfigField
nameField = Accepted "name"

{- The name of a sameas remote is stored using this key instead. 
 - This prevents old versions of git-annex getting confused. -}
sameasNameField :: RemoteConfigField
sameasNameField = Accepted "sameas-name"

lookupName :: RemoteConfig -> Maybe String
lookupName c = fmap fromProposedAccepted $
	M.lookup nameField c <|> M.lookup sameasNameField c

instance RemoteNameable RemoteConfig where
	getRemoteName c = fromMaybe "" (lookupName c)

{- The uuid that a sameas remote is the same as is stored in this key. -}
sameasUUIDField :: RemoteConfigField
sameasUUIDField = Accepted "sameas-uuid"

{- The type of a remote is stored in its config using this key. -}
typeField :: RemoteConfigField
typeField = Accepted "type"

autoEnableField :: RemoteConfigField
autoEnableField = Accepted "autoenable"

encryptionField :: RemoteConfigField
encryptionField = Accepted "encryption"

macField :: RemoteConfigField
macField = Accepted "mac"

cipherField :: RemoteConfigField
cipherField = Accepted "cipher"

cipherkeysField :: RemoteConfigField
cipherkeysField = Accepted "cipherkeys"

pubkeysField :: RemoteConfigField
pubkeysField = Accepted "pubkeys"

chunkField :: RemoteConfigField
chunkField = Accepted "chunk"

chunksizeField :: RemoteConfigField
chunksizeField = Accepted "chunksize"

embedCredsField :: RemoteConfigField
embedCredsField = Accepted "embedcreds"

exportTreeField :: RemoteConfigField
exportTreeField = Accepted "exporttree"

importTreeField :: RemoteConfigField
importTreeField = Accepted "importtree"

exportTree :: ParsedRemoteConfig -> Bool
exportTree = fromMaybe False . getRemoteConfigValue exportTreeField

importTree :: ParsedRemoteConfig -> Bool
importTree = fromMaybe False . getRemoteConfigValue importTreeField

{- Parsers for fields that are common to all special remotes. -}
commonFieldParsers :: [RemoteConfigFieldParser]
commonFieldParsers =
	[ optionalStringParser nameField
	, optionalStringParser sameasNameField
	, optionalStringParser sameasUUIDField
	, optionalStringParser typeField
	, trueFalseParser autoEnableField False
	]

{- A remote with sameas-uuid set will inherit these values from the config
 - of that uuid. These values cannot be overridden in the remote's config. -}
sameasInherits :: S.Set RemoteConfigField
sameasInherits = S.fromList
	-- encryption configuration is necessarily the same for two
	-- remotes that access the same data store
	[ encryptionField
	, macField
	, cipherField
	, cipherkeysField
	, pubkeysField
	-- legacy chunking was either enabled or not, so has to be the same
	-- across configs for remotes that access the same data
	-- (new-style chunking does not have that limitation)
	, chunksizeField
	]

{- Each RemoteConfig that has a sameas-uuid inherits some fields
 - from it. Such fields can only be set by inheritance; the RemoteConfig
 - cannot provide values from them. -}
addSameasInherited :: M.Map UUID RemoteConfig -> RemoteConfig -> RemoteConfig
addSameasInherited m c = case findSameasUUID c of
	Nothing -> c
	Just (Sameas sameasuuid) -> case M.lookup sameasuuid m of
		Nothing -> c
		Just parentc -> 
			M.withoutKeys c sameasInherits			
				`M.union`
			M.restrictKeys parentc sameasInherits

findSameasUUID :: RemoteConfig -> Maybe (Sameas UUID)
findSameasUUID c = Sameas . toUUID . fromProposedAccepted
	<$> M.lookup sameasUUIDField c

{- Remove any fields inherited from a sameas-uuid. When storing a
 - RemoteConfig, those fields don't get stored, since they were already
 - inherited. -}
removeSameasInherited :: RemoteConfig -> RemoteConfig
removeSameasInherited c = case M.lookup sameasUUIDField c of
	Nothing -> c
	Just _ -> M.withoutKeys c sameasInherits

{- Finds remote uuids with matching RemoteConfig. -}
findByRemoteConfig :: (RemoteConfig -> Bool) -> M.Map UUID RemoteConfig -> [(UUID, RemoteConfig, Maybe (ConfigFrom UUID))]
findByRemoteConfig matching = map sameasuuid . filter (matching . snd) . M.toList
  where
	sameasuuid (u, c) = case M.lookup sameasUUIDField c of
		Nothing -> (u, c, Nothing)
		Just u' -> (toUUID (fromProposedAccepted u'), c, Just (ConfigFrom u))

{- Extracts a value from ParsedRemoteConfig. -}
getRemoteConfigValue :: HasCallStack => Typeable v => RemoteConfigField -> ParsedRemoteConfig -> Maybe v
getRemoteConfigValue f m = case M.lookup f m of
	Just (RemoteConfigValue v) -> case cast v of
		Just v' -> Just v'
		Nothing -> error $ unwords
			[ "getRemoteConfigValue"
			, fromProposedAccepted f
			, "found value of unexpected type"
			, show (typeOf v) ++ "."
			, "This is a bug in git-annex!"
			]
	Nothing -> Nothing

parseRemoteConfig :: RemoteConfig -> RemoteConfigParser -> Either String ParsedRemoteConfig
parseRemoteConfig c rpc =
	go [] (M.filterWithKey notaccepted c) (remoteConfigFieldParsers rpc ++ commonFieldParsers)
  where
	go l c' []
		| remoteConfigRestPassthrough rpc = Right $ M.fromList $
			l ++ map (uncurry passthrough) (M.toList c')
		| M.null c' = Right (M.fromList l)
		| otherwise = Left $ "Unexpected fields: " ++
			unwords (map fromProposedAccepted (M.keys c'))
	go l c' ((f, p):rest) = do
		v <- p (M.lookup f c) c
		case v of
			Just v' -> go ((f,v'):l) (M.delete f c') rest
			Nothing -> go l (M.delete f c') rest
	
	passthrough f v = (f, RemoteConfigValue (fromProposedAccepted v))

	notaccepted (Proposed _) _ = True
	notaccepted (Accepted _) _ = False

optionalStringParser :: RemoteConfigField -> RemoteConfigFieldParser
optionalStringParser f = (f, p)
  where
	p (Just v) _c = Right (Just (RemoteConfigValue (fromProposedAccepted v)))
	p Nothing _c = Right Nothing

yesNoParser :: RemoteConfigField -> Bool -> RemoteConfigFieldParser
yesNoParser = genParser yesNo "yes or no"

trueFalseParser :: RemoteConfigField -> Bool -> RemoteConfigFieldParser
trueFalseParser = genParser Git.Config.isTrueFalse "true or false"

genParser
	:: Typeable t
	=> (String -> Maybe t)
	-> String -- ^ description of the value
	-> RemoteConfigField
	-> t -- ^ fallback value
	-> RemoteConfigFieldParser
genParser parse desc f fallback = (f, p)
  where
	p Nothing _c = Right (Just (RemoteConfigValue fallback))
	p (Just v) _c = case parse (fromProposedAccepted v) of
		Just b -> Right (Just (RemoteConfigValue b))
		Nothing -> case v of
			Accepted _ -> Right (Just (RemoteConfigValue fallback))
			Proposed _ -> Left $
				"Bad value for " ++ fromProposedAccepted f ++
				" (expected " ++ desc ++ ")"

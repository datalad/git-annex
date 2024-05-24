{- git-annex special remote configuration
 -
 - Copyright 2019-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Annex.SpecialRemote.Config where

import Common
import Types.Remote (configParser)
import Types
import Types.UUID
import Types.ProposedAccepted
import Types.RemoteConfig
import Types.GitConfig
import Config.Cost

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Read
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

costField :: RemoteConfigField
costField = Accepted "cost"

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

preferreddirField :: RemoteConfigField
preferreddirField = Accepted "preferreddir"

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
		(FieldDesc "name for the special remote")
	, optionalStringParser sameasNameField HiddenField
	, optionalStringParser sameasUUIDField HiddenField
	, autoEnableFieldParser
	, costParser costField
		(FieldDesc "default cost of this special remote")
	, optionalStringParser preferreddirField
		(FieldDesc "directory whose content is preferred")
	] ++ essentialFieldParsers

{- Parsers for fields that are common to all special remotes, and are
 - also essential to include in eg, annex:: urls. -}
essentialFieldParsers :: [RemoteConfigFieldParser]
essentialFieldParsers =
	[ optionalStringParser typeField
		(FieldDesc "type of special remote")
	, yesNoParser exportTreeField (Just False)
		(FieldDesc "export trees of files to this remote")
	, yesNoParser importTreeField (Just False)
		(FieldDesc "import trees of files from this remote")
	]

autoEnableFieldParser :: RemoteConfigFieldParser
autoEnableFieldParser = trueFalseParser autoEnableField (Just False)
	(FieldDesc "automatically enable special remote")

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
	, chunksizeField
	-- (new-style chunking does not have that limitation)
	-- but there is no benefit to picking a different chunk size
	-- for the sameas remote, since it's reading whatever chunks were
	-- stored
	, chunkField
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
getRemoteConfigValue f (ParsedRemoteConfig m _) = case M.lookup f m of
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

{- Gets all fields that remoteConfigRestPassthrough matched. -}
getRemoteConfigPassedThrough :: ParsedRemoteConfig -> M.Map RemoteConfigField String
getRemoteConfigPassedThrough (ParsedRemoteConfig m _) = 
	flip M.mapMaybe m $ \(RemoteConfigValue v) ->
		case cast v of
			Just (PassedThrough s) -> Just s
			Nothing -> Nothing

newtype PassedThrough = PassedThrough String

parsedRemoteConfig :: RemoteType -> RemoteConfig -> Annex ParsedRemoteConfig
parsedRemoteConfig t c = either (const emptycfg) id . parseRemoteConfig c 
	<$> configParser t c
  where
	emptycfg = ParsedRemoteConfig mempty c

parseRemoteConfig :: RemoteConfig -> RemoteConfigParser -> Either String ParsedRemoteConfig
parseRemoteConfig c rpc =
	go [] c (remoteConfigFieldParsers rpc ++ commonFieldParsers)
  where
	go l c' [] =
		let (passover, leftovers) = partition
			(maybe (const False) fst (remoteConfigRestPassthrough rpc) . fst)
			(M.toList c')
		    leftovers' = filter (notaccepted . fst) leftovers
		in if not (null leftovers')
			then Left $ "Unexpected parameters: " ++
				unwords (map (fromProposedAccepted . fst) leftovers')
			else
				let m = M.fromList $
					l ++ map (uncurry passthrough) passover
				in Right (ParsedRemoteConfig m c)
	go l c' (p:rest) = do
		let f = parserForField p
		(valueParser p) (M.lookup f c) c >>= \case
			Just v -> go ((f,v):l) (M.delete f c') rest
			Nothing -> go l (M.delete f c') rest
	
	passthrough f v = (f, RemoteConfigValue (PassedThrough (fromProposedAccepted v)))
	
	notaccepted (Proposed _) = True
	notaccepted (Accepted _) = False

optionalStringParser :: RemoteConfigField -> FieldDesc -> RemoteConfigFieldParser
optionalStringParser f fielddesc = RemoteConfigFieldParser
	{ parserForField = f 
	, valueParser = p
	, fieldDesc = fielddesc
	, valueDesc = Nothing
	}
  where
	p (Just v) _c = Right (Just (RemoteConfigValue (fromProposedAccepted v)))
	p Nothing _c = Right Nothing

yesNoParser :: RemoteConfigField -> Maybe Bool -> FieldDesc -> RemoteConfigFieldParser
yesNoParser f mdef fd = genParser yesno f mdef fd
	(Just (ValueDesc "yes or no"))
  where
	yesno "yes" = Just True
	yesno "no" = Just False
	yesno _ = Nothing

trueFalseParser :: RemoteConfigField -> Maybe Bool -> FieldDesc -> RemoteConfigFieldParser
trueFalseParser f mdef fd = genParser trueFalseParser' f mdef fd
	(Just (ValueDesc "true or false"))

-- Not using Git.Config.isTrueFalse because git supports
-- a lot of other values for true and false in its configs,
-- and this is not a git config and we want to avoid that mess.
trueFalseParser' :: String -> Maybe Bool
trueFalseParser' "true" = Just True
trueFalseParser' "false" = Just False
trueFalseParser' _ = Nothing

costParser :: RemoteConfigField -> FieldDesc -> RemoteConfigFieldParser
costParser f fd = genParser readcost f Nothing fd
	(Just (ValueDesc "a number"))
  where
	readcost :: String -> Maybe Cost
	readcost = readMaybe

genParser
	:: Typeable t
	=> (String -> Maybe t)
	-> RemoteConfigField
	-> Maybe t -- ^ default if not configured
	-> FieldDesc
	-> Maybe ValueDesc
	-> RemoteConfigFieldParser
genParser parse f mdef fielddesc valuedesc = RemoteConfigFieldParser	
	{ parserForField = f
	, valueParser = p
	, fieldDesc = fielddesc
	, valueDesc = valuedesc
	}
  where
	p Nothing _c = Right (fmap RemoteConfigValue mdef)
	p (Just v) _c = case parse (fromProposedAccepted v) of
		Just b -> Right (Just (RemoteConfigValue b))
		Nothing -> case v of
			Accepted _ -> Right (fmap RemoteConfigValue mdef)
			Proposed _ -> Left $
				"Bad value for " ++ fromProposedAccepted f ++
				case valuedesc of
					Just (ValueDesc vd) ->
						" (expected " ++ vd ++ ")"
					Nothing -> ""

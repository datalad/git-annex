{- P2P protocol over HTTP,
 - data types for servant not including the servant API
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module P2P.Http.Types where

import Annex.Common
import qualified P2P.Protocol as P2P
import Utility.MonotonicClock

#ifdef WITH_SERVANT
import Servant
import Data.Aeson hiding (Key)
import Text.Read (readMaybe)
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Codec.Binary.Base64Url as B64
import Data.Char
import Control.DeepSeq
import GHC.Generics (Generic)

data V3 = V3 deriving (Show)
data V2 = V2 deriving (Show)
data V1 = V1 deriving (Show)
data V0 = V0 deriving (Show)

class APIVersion v where
	protocolVersion :: v -> P2P.ProtocolVersion

instance APIVersion V3 where protocolVersion _ = P2P.ProtocolVersion 3
instance APIVersion V2 where protocolVersion _ = P2P.ProtocolVersion 2
instance APIVersion V1 where protocolVersion _ = P2P.ProtocolVersion 1
instance APIVersion V0 where protocolVersion _ = P2P.ProtocolVersion 0

-- Keys, UUIDs, and filenames can be base64 encoded since Servant uses 
-- Text and so needs UTF-8.
newtype B64Key = B64Key Key
	deriving (Show)

newtype B64FilePath = B64FilePath RawFilePath
	deriving (Show)

associatedFileToB64FilePath :: AssociatedFile -> Maybe B64FilePath
associatedFileToB64FilePath (AssociatedFile Nothing) = Nothing
associatedFileToB64FilePath (AssociatedFile (Just f)) = Just (B64FilePath f)

b64FilePathToAssociatedFile :: Maybe B64FilePath -> AssociatedFile
b64FilePathToAssociatedFile Nothing = AssociatedFile Nothing
b64FilePathToAssociatedFile (Just (B64FilePath f)) = AssociatedFile (Just f)

newtype B64UUID t = B64UUID { fromB64UUID :: UUID }
	deriving (Show, Ord, Eq, Generic, NFData)

encodeB64Text :: B.ByteString -> T.Text
encodeB64Text b = case TE.decodeUtf8' b of
	Right t
		| (snd <$> B.unsnoc b) == Just closebracket 
			&& (fst <$> B.uncons b) == Just openbracket ->
				b64wrapped
		| otherwise -> t
	Left _ -> b64wrapped
  where
	b64wrapped = TE.decodeUtf8 $ "[" <> B64.encode b <> "]"
	openbracket = fromIntegral (ord '[')
	closebracket = fromIntegral (ord ']')

decodeB64Text :: T.Text -> Either T.Text B.ByteString
decodeB64Text t = 
	case T.unsnoc t of
		Just (t', lastc) | lastc == ']' ->
			case T.uncons t' of
				Just (firstc, t'') | firstc == '[' ->
					case B64.decode (TE.encodeUtf8 t'') of
						Right b -> Right b
						Left _ -> Left "unable to base64 decode [] wrapped value"
				_ -> Right (TE.encodeUtf8 t)
		_ -> Right (TE.encodeUtf8 t)

-- Phantom types.
data ClientSide
data ServerSide
data Bypass
data Plus
data Lock

type LockID = B64UUID Lock

newtype DataLength = DataLength Integer
	deriving (Show)

newtype CheckPresentResult = CheckPresentResult Bool
	deriving (Show)

newtype RemoveResult = RemoveResult Bool
	deriving (Show)

data RemoveResultPlus = RemoveResultPlus Bool [B64UUID Plus]
	deriving (Show)

newtype GetTimestampResult = GetTimestampResult Timestamp
	deriving (Show)

newtype PutResult = PutResult Bool
	deriving (Eq, Show)

data PutResultPlus = PutResultPlus Bool [B64UUID Plus]
	deriving (Show)

data PutOffsetResult
	= PutOffsetResult Offset
	| PutOffsetResultAlreadyHave
	deriving (Show)

data PutOffsetResultPlus 
	= PutOffsetResultPlus Offset
	| PutOffsetResultAlreadyHavePlus [B64UUID Plus]
	deriving (Show, Generic, NFData)

newtype Offset = Offset P2P.Offset
	deriving (Show, Generic, NFData)

newtype Timestamp = Timestamp MonotonicTimestamp
	deriving (Show)

data LockResult = LockResult Bool (Maybe LockID)
	deriving (Show, Generic, NFData)

newtype UnlockRequest = UnlockRequest Bool
	deriving (Show, Generic, NFData)

-- Not using servant's built-in basic authentication support,
-- because whether authentication is needed depends on server
-- configuration.
data Auth = Auth B.ByteString B.ByteString
	deriving (Show, Generic, NFData, Eq, Ord)

#ifdef WITH_SERVANT

instance ToHttpApiData Auth where
	toHeader (Auth u p) = "Basic " <> B64.encode (u <> ":" <> p)
#if MIN_VERSION_text(2,0,0)
	toUrlPiece = TE.decodeUtf8Lenient . toHeader
#else
	toUrlPiece = TE.decodeUtf8With (\_ _ -> Just '\xfffd') . toHeader
#endif

instance FromHttpApiData Auth where
	parseHeader h =
		let (b, rest) = B.break (isSpace . chr . fromIntegral) h
		in if map toLower (decodeBS b) == "basic"
			then case B64.decode (B.dropWhile (isSpace . chr . fromIntegral) rest) of
				Right v -> case B.split (fromIntegral (ord ':')) v of
					(u:ps) -> Right $
						Auth u (B.intercalate ":" ps)
					_ -> bad
				Left _ -> bad
			else bad
	  where
		bad = Left "invalid basic auth header"
	parseUrlPiece = parseHeader . encodeBS . T.unpack

newtype ConnectionKeepAlive = ConnectionKeepAlive T.Text

connectionKeepAlive :: ConnectionKeepAlive
connectionKeepAlive = ConnectionKeepAlive "Keep-Alive"

newtype KeepAlive = KeepAlive T.Text

keepAlive :: KeepAlive
keepAlive = KeepAlive "timeout=1200"

instance ToHttpApiData ConnectionKeepAlive where
	toUrlPiece (ConnectionKeepAlive t) = t

instance FromHttpApiData ConnectionKeepAlive where
	parseUrlPiece = Right . ConnectionKeepAlive

instance ToHttpApiData KeepAlive where
	toUrlPiece (KeepAlive t) = t

instance FromHttpApiData KeepAlive where
	parseUrlPiece = Right . KeepAlive

instance ToHttpApiData V3 where toUrlPiece _ = "v3"
instance ToHttpApiData V2 where toUrlPiece _ = "v2"
instance ToHttpApiData V1 where toUrlPiece _ = "v1"
instance ToHttpApiData V0 where toUrlPiece _ = "v0"

instance FromHttpApiData V3 where parseUrlPiece = parseAPIVersion V3 "v3"
instance FromHttpApiData V2 where parseUrlPiece = parseAPIVersion V2 "v2"
instance FromHttpApiData V1 where parseUrlPiece = parseAPIVersion V1 "v1"
instance FromHttpApiData V0 where parseUrlPiece = parseAPIVersion V0 "v0"

parseAPIVersion :: v -> T.Text -> T.Text -> Either T.Text v
parseAPIVersion v need t
	| t == need = Right v
	| otherwise = Left "bad version"

instance ToHttpApiData B64Key where
	toUrlPiece (B64Key k) = encodeB64Text (serializeKey' k)

instance FromHttpApiData B64Key where
	parseUrlPiece t = case decodeB64Text t of
		Right b -> maybe (Left "key parse error") (Right . B64Key)
			(deserializeKey' b)
		Left err -> Left err

instance ToHttpApiData (B64UUID t) where
	toUrlPiece (B64UUID u) = encodeB64Text (fromUUID u)

instance FromHttpApiData (B64UUID t) where
	parseUrlPiece t = case decodeB64Text t of
		Right b -> case toUUID b of
			u@(UUID _) -> Right (B64UUID u)
			NoUUID -> Left "empty UUID"
		Left err -> Left err

instance ToHttpApiData B64FilePath where
	toUrlPiece (B64FilePath f) = encodeB64Text f

instance FromHttpApiData B64FilePath where
	parseUrlPiece t = case decodeB64Text t of
		Right b -> Right (B64FilePath b)
		Left err -> Left err

instance ToHttpApiData Offset where
	toUrlPiece (Offset (P2P.Offset n)) = T.pack (show n)

instance FromHttpApiData Offset where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "offset parse error"
		Just n -> Right (Offset (P2P.Offset n))

instance ToHttpApiData Timestamp where
	toUrlPiece (Timestamp (MonotonicTimestamp n)) = T.pack (show n)

instance FromHttpApiData Timestamp where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "timestamp parse error"
		Just n -> Right (Timestamp (MonotonicTimestamp n))

instance ToHttpApiData DataLength where
	toUrlPiece (DataLength n) = T.pack (show n)

instance FromHttpApiData DataLength where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "X-git-annex-data-length parse error"
		Just n -> Right (DataLength n)

instance ToJSON PutResult where
	toJSON (PutResult b) =
		object ["stored" .= b]

instance FromJSON PutResult where
	parseJSON = withObject "PutResult" $ \v -> PutResult
		<$> v .: "stored"

instance ToJSON PutResultPlus where
	toJSON (PutResultPlus b us) = object
		[ "stored" .= b
		, "plusuuids" .= plusList us
		]

instance FromJSON PutResultPlus where
	parseJSON = withObject "PutResultPlus" $ \v -> PutResultPlus
		<$> v .: "stored"
		<*> v .: "plusuuids"

instance ToJSON CheckPresentResult where
	toJSON (CheckPresentResult b) = object
		["present" .= b]

instance FromJSON CheckPresentResult where
	parseJSON = withObject "CheckPresentResult" $ \v -> CheckPresentResult
		<$> v .: "present"

instance ToJSON RemoveResult where
	toJSON (RemoveResult b) = object
		["removed" .= b]

instance FromJSON RemoveResult where
	parseJSON = withObject "RemoveResult" $ \v -> RemoveResult
		<$> v .: "removed"

instance ToJSON RemoveResultPlus where
	toJSON (RemoveResultPlus b us) = object
		[ "removed" .= b
		, "plusuuids" .= plusList us
		]

instance FromJSON RemoveResultPlus where
	parseJSON = withObject "RemoveResultPlus" $ \v -> RemoveResultPlus
		<$> v .: "removed"
		<*> v .: "plusuuids"

instance ToJSON GetTimestampResult where
	toJSON (GetTimestampResult (Timestamp (MonotonicTimestamp t))) = object
		["timestamp" .= t]

instance FromJSON GetTimestampResult where
	parseJSON = withObject "GetTimestampResult" $ \v ->
		GetTimestampResult . Timestamp . MonotonicTimestamp
			<$> v .: "timestamp"

instance ToJSON PutOffsetResult where
	toJSON (PutOffsetResult (Offset (P2P.Offset o))) = object
		["offset" .= o]
	toJSON PutOffsetResultAlreadyHave = object
		["alreadyhave" .= True]

instance FromJSON PutOffsetResult where
	parseJSON = withObject "PutOffsetResult" $ \v ->
		(PutOffsetResult
			<$> (Offset . P2P.Offset <$> v .: "offset"))
		<|> (mkalreadyhave
			<$> (v .: "alreadyhave"))
	  where
		mkalreadyhave :: Bool -> PutOffsetResult
		mkalreadyhave _ = PutOffsetResultAlreadyHave

instance ToJSON PutOffsetResultPlus where
	toJSON (PutOffsetResultPlus (Offset (P2P.Offset o))) = object
		[ "offset" .= o ]
	toJSON (PutOffsetResultAlreadyHavePlus us) = object
		[ "alreadyhave" .= True
		, "plusuuids" .= plusList us
		]

instance FromJSON PutOffsetResultPlus where
	parseJSON = withObject "PutOffsetResultPlus" $ \v ->
		(PutOffsetResultPlus
			<$> (Offset . P2P.Offset <$> v .: "offset"))
		<|> (mkalreadyhave
			<$> (v .: "alreadyhave")
			<*> (v .: "plusuuids"))
	  where
		mkalreadyhave :: Bool -> [B64UUID Plus] -> PutOffsetResultPlus
		mkalreadyhave _ us = PutOffsetResultAlreadyHavePlus us

instance FromJSON (B64UUID t) where
	parseJSON (String t) = case decodeB64Text t of
		Right s -> pure (B64UUID (toUUID s))
		Left _ -> mempty
	parseJSON _ = mempty

instance ToJSON LockResult where
	toJSON (LockResult v (Just (B64UUID lck))) = object
		[ "locked" .= v
		, "lockid" .= encodeB64Text (fromUUID lck)
		]
	toJSON (LockResult v Nothing) = object
		[ "locked" .= v
		]

instance FromJSON LockResult where
	parseJSON = withObject "LockResult" $ \v -> LockResult
		<$> v .: "locked"
		<*> v .:? "lockid"

instance ToJSON UnlockRequest where
	toJSON (UnlockRequest v) = object
		["unlock" .= v]

instance FromJSON UnlockRequest where
	parseJSON = withObject "UnlockRequest" $ \v -> UnlockRequest
		<$> v .: "unlock"

plusList :: [B64UUID Plus] -> [String]
plusList = map (\(B64UUID u) -> fromUUID u)

class PlusClass plus unplus where
	dePlus :: plus -> unplus
	plus :: unplus -> plus

instance PlusClass RemoveResultPlus RemoveResult where
	dePlus (RemoveResultPlus b _) = RemoveResult b
	plus (RemoveResult b) = RemoveResultPlus b mempty

instance PlusClass PutResultPlus PutResult where
	dePlus (PutResultPlus b _) = PutResult b
	plus (PutResult b) = PutResultPlus b mempty

instance PlusClass PutOffsetResultPlus PutOffsetResult where
	dePlus (PutOffsetResultPlus o) = PutOffsetResult o
	dePlus (PutOffsetResultAlreadyHavePlus _) = PutOffsetResultAlreadyHave
	plus (PutOffsetResult o) = PutOffsetResultPlus o
	plus PutOffsetResultAlreadyHave = PutOffsetResultAlreadyHavePlus []

#endif

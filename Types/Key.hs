{- git-annex Key data type
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Types.Key (
	KeyData(..),
	Key,
	fromKey,
	mkKey,
	alterKey,
	isKeyPrefix,
	splitKeyNameExtension,
	keyParser,
	keySerialization,
	AssociatedFile(..),
	KeyVariety(..),
	HasExt(..),
	HashSize(..),
	hasExt,
	sameExceptExt,
	formatKeyVariety,
	parseKeyVariety,
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Utility.FileSystemEncoding
import Data.List
import Data.Char
import System.Posix.Types
import Foreign.C.Types
import Data.Monoid
import Control.Applicative
import GHC.Generics
import Control.DeepSeq
import Prelude

{- A Key has a unique name, which is derived from a particular backend,
 - and may contain other optional metadata. -}
data KeyData = Key
	{ keyName :: S.ByteString
	, keyVariety :: KeyVariety
	, keySize :: Maybe Integer
	, keyMtime :: Maybe EpochTime
	, keyChunkSize :: Maybe Integer
	, keyChunkNum :: Maybe Integer
	} deriving (Eq, Ord, Read, Show, Generic)

instance NFData KeyData

{- Caching the seralization of a key is an optimization.
 -
 - This constructor is not exported, and all smart constructors maintain
 - the serialization.
 -}
data Key = MkKey
	{ keyData :: KeyData
	, keySerialization :: S.ByteString
	} deriving (Show, Generic)

instance Eq Key where
	-- comparing the serialization would be unncessary work
	a == b = keyData a == keyData b

instance Ord Key where
	compare a b = compare (keyData a) (keyData b)

instance NFData Key

{- Access a field of data from the KeyData. -}
{-# INLINE fromKey #-}
fromKey :: (KeyData -> a) -> Key -> a
fromKey f = f . keyData

{- Smart constructor for a Key. The provided KeyData has all values empty. -}
mkKey :: (KeyData -> KeyData) -> Key
mkKey f =
	let d = f stub
	in MkKey d (mkKeySerialization d)
  where
	stub = Key
		{ keyName = mempty
		, keyVariety = OtherKey mempty
		, keySize = Nothing
		, keyMtime = Nothing
		, keyChunkSize = Nothing
		, keyChunkNum = Nothing
		}

{- Alter a Key's data. -}
alterKey :: Key -> (KeyData -> KeyData) -> Key
alterKey k f = 
	let d = f (keyData k)
	in MkKey d (mkKeySerialization d)

-- Checks if a string looks like at least the start of a key.
isKeyPrefix :: String -> Bool
isKeyPrefix s = [fieldSep, fieldSep] `isInfixOf` s

fieldSep :: Char
fieldSep = '-'

mkKeySerialization :: KeyData -> S.ByteString
mkKeySerialization = L.toStrict
    	. toLazyByteStringWith (safeStrategy 128 smallChunkSize) L.empty
	. buildKeyData

{- Builds a ByteString from a KeyData.
 -
 - The name field is always shown last, separated by doubled fieldSeps,
 - and is the only field allowed to contain the fieldSep.
 -}
buildKeyData :: KeyData -> Builder
buildKeyData k = byteString (formatKeyVariety (keyVariety k))
	<> 's' ?: (integerDec <$> keySize k)
	<> 'm' ?: (integerDec . (\(CTime t) -> fromIntegral t) <$> keyMtime k)
	<> 'S' ?: (integerDec <$> keyChunkSize k)
	<> 'C' ?: (integerDec <$> keyChunkNum k)
	<> sepbefore (sepbefore (byteString (keyName k)))
  where
	sepbefore s = char7 fieldSep <> s
	c ?: (Just b) = sepbefore (char7 c <> b)
	_ ?: Nothing = mempty

{- This is a strict parser for security reasons; a key
 - can contain only 4 fields, which all consist only of numbers.
 - Any key containing other fields, or non-numeric data will fail
 - to parse.
 -
 - If a key contained non-numeric fields, they could be used to
 - embed data used in a SHA1 collision attack, which would be a
 - problem since the keys are committed to git.
 -}
keyParser :: A.Parser Key
keyParser = do
	-- key variety cannot be empty
	v <- (parseKeyVariety <$> A8.takeWhile1 (/= fieldSep))
	s <- parsesize
	m <- parsemtime
	cs <- parsechunksize
	cn <- parsechunknum
	_ <- A8.char fieldSep
	_ <- A8.char fieldSep
	n <- A.takeByteString
	if validKeyName v n
		then 
			let d = Key
				{ keyName = n
				, keyVariety = v
				, keySize = s
				, keyMtime = m
				, keyChunkSize = cs
				, keyChunkNum = cn
				}
			in pure $ MkKey d (mkKeySerialization d)
		else fail "invalid keyName"
  where
	parseopt p = (Just <$> (A8.char fieldSep *> p)) <|> pure Nothing
	parsesize = parseopt $ A8.char 's' *> A8.decimal
	parsemtime = parseopt $ CTime <$> (A8.char 'm' *> A8.decimal)
	parsechunksize = parseopt $ A8.char 'S' *> A8.decimal
	parsechunknum = parseopt $ A8.char 'C' *> A8.decimal

{- Limits the length of the extension in the keyName to mitigate against
 - SHA1 collision attacks.
 -
 - In such an attack, the extension of the key could be made to contain
 - the collision generation data, with the result that a signed git commit
 - including such keys would not be secure.
 -
 - The maximum extension length ever generated for such a key was 8
 - characters, but they may be unicode which could use up to 4 bytes each,
 - so 32 bytes. 64 bytes is used here to give a little future wiggle-room. 
 - The SHA1 common-prefix attack needs 128 bytes of data.
 -}
validKeyName :: KeyVariety -> S.ByteString -> Bool
validKeyName kv name
	| hasExt kv = 
		let ext = snd $ splitKeyNameExtension' name
		in S.length ext <= 64
	| otherwise = True

{- This splits any extension out of the keyName, returning the 
 - keyName minus extension, and the extension (including leading dot).
 -}
splitKeyNameExtension :: Key -> (S.ByteString, S.ByteString)
splitKeyNameExtension = splitKeyNameExtension' . keyName . keyData

splitKeyNameExtension' :: S.ByteString -> (S.ByteString, S.ByteString)
splitKeyNameExtension' keyname = S8.span (/= '.') keyname

{- A filename may be associated with a Key. -}
newtype AssociatedFile = AssociatedFile (Maybe RawFilePath)
	deriving (Show, Eq, Ord)

{- There are several different varieties of keys. -}
data KeyVariety
	= SHA2Key HashSize HasExt
	| SHA3Key HashSize HasExt
	| SKEINKey HashSize HasExt
	| Blake2bKey HashSize HasExt
	| Blake2bpKey HashSize HasExt
	| Blake2sKey HashSize HasExt
	| Blake2spKey HashSize HasExt
	| SHA1Key HasExt
	| MD5Key HasExt
	| WORMKey
	| URLKey
	-- A key that is handled by some external backend.
	| ExternalKey S.ByteString HasExt
 	-- Some repositories may contain keys of other varieties,
	-- which can still be processed to some extent.
	| OtherKey S.ByteString
	deriving (Eq, Ord, Read, Show, Generic)

instance NFData KeyVariety

{- Some varieties of keys may contain an extension at the end of the
 - keyName -}
newtype HasExt = HasExt Bool
	deriving (Eq, Ord, Read, Show, Generic)

instance NFData HasExt

newtype HashSize = HashSize Int
	deriving (Eq, Ord, Read, Show, Generic)

instance NFData HashSize

hasExt :: KeyVariety -> Bool
hasExt (SHA2Key _ (HasExt b)) = b
hasExt (SHA3Key _ (HasExt b)) = b
hasExt (SKEINKey _ (HasExt b)) = b
hasExt (Blake2bKey _ (HasExt b)) = b
hasExt (Blake2bpKey _ (HasExt b)) = b
hasExt (Blake2sKey _ (HasExt b)) = b
hasExt (Blake2spKey _ (HasExt b)) = b
hasExt (SHA1Key (HasExt b)) = b
hasExt (MD5Key (HasExt b)) = b
hasExt WORMKey = False
hasExt URLKey = False
hasExt (ExternalKey _ (HasExt b)) = b
hasExt (OtherKey s) = (snd <$> S8.unsnoc s) == Just 'E'

sameExceptExt :: KeyVariety -> KeyVariety -> Bool
sameExceptExt (SHA2Key sz1 _) (SHA2Key sz2 _) = sz1 == sz2
sameExceptExt (SHA3Key sz1 _) (SHA3Key sz2 _) = sz1 == sz2
sameExceptExt (SKEINKey sz1 _) (SKEINKey sz2 _) = sz1 == sz2
sameExceptExt (Blake2bKey sz1 _) (Blake2bKey sz2 _) = sz1 == sz2
sameExceptExt (Blake2bpKey sz1 _) (Blake2bpKey sz2 _) = sz1 == sz2
sameExceptExt (Blake2sKey sz1 _) (Blake2sKey sz2 _) = sz1 == sz2
sameExceptExt (Blake2spKey sz1 _) (Blake2spKey sz2 _) = sz1 == sz2
sameExceptExt (SHA1Key _) (SHA1Key _) = True
sameExceptExt (MD5Key _) (MD5Key _) = True
sameExceptExt _ _ = False

formatKeyVariety :: KeyVariety -> S.ByteString
formatKeyVariety v = case v of
	SHA2Key sz e -> adde e (addsz sz "SHA")
	SHA3Key sz e -> adde e (addsz sz "SHA3_")
	SKEINKey sz e -> adde e (addsz sz "SKEIN")
	Blake2bKey sz e -> adde e (addsz sz "BLAKE2B")
	Blake2bpKey sz e -> adde e (addsz sz "BLAKE2BP")
	Blake2sKey sz e -> adde e (addsz sz "BLAKE2S")
	Blake2spKey sz e -> adde e (addsz sz "BLAKE2SP")
	SHA1Key e -> adde e "SHA1"
	MD5Key e -> adde e "MD5"
	WORMKey -> "WORM"
	URLKey -> "URL"
	ExternalKey s e -> adde e ("X" <> s)
	OtherKey s -> s
  where
	adde (HasExt False) s = s
	adde (HasExt True) s = s <> "E"
	addsz (HashSize n) s = s <> case n of
		256 -> "256"
		512 -> "512"
		224 -> "224"
		384 -> "384"
		160 -> "160"
		-- This is relatively slow, which is why the common hash
		-- sizes are hardcoded above.
		_ -> S8.pack (show n)

parseKeyVariety :: S.ByteString -> KeyVariety
parseKeyVariety "SHA256"       = SHA2Key (HashSize 256) (HasExt False)
parseKeyVariety "SHA256E"      = SHA2Key (HashSize 256) (HasExt True)
parseKeyVariety "SHA512"       = SHA2Key (HashSize 512) (HasExt False)
parseKeyVariety "SHA512E"      = SHA2Key (HashSize 512) (HasExt True)
parseKeyVariety "SHA224"       = SHA2Key (HashSize 224) (HasExt False)
parseKeyVariety "SHA224E"      = SHA2Key (HashSize 224) (HasExt True)
parseKeyVariety "SHA384"       = SHA2Key (HashSize 384) (HasExt False)
parseKeyVariety "SHA384E"      = SHA2Key (HashSize 384) (HasExt True)
parseKeyVariety "SHA3_512"     = SHA3Key (HashSize 512) (HasExt False)
parseKeyVariety "SHA3_512E"    = SHA3Key (HashSize 512) (HasExt True)
parseKeyVariety "SHA3_384"     = SHA3Key (HashSize 384) (HasExt False)
parseKeyVariety "SHA3_384E"    = SHA3Key (HashSize 384) (HasExt True)
parseKeyVariety "SHA3_256"     = SHA3Key (HashSize 256) (HasExt False)
parseKeyVariety "SHA3_256E"    = SHA3Key (HashSize 256) (HasExt True)
parseKeyVariety "SHA3_224"     = SHA3Key (HashSize 224) (HasExt False)
parseKeyVariety "SHA3_224E"    = SHA3Key (HashSize 224) (HasExt True)
parseKeyVariety "SKEIN512"     = SKEINKey (HashSize 512) (HasExt False)
parseKeyVariety "SKEIN512E"    = SKEINKey (HashSize 512) (HasExt True)
parseKeyVariety "SKEIN256"     = SKEINKey (HashSize 256) (HasExt False)
parseKeyVariety "SKEIN256E"    = SKEINKey (HashSize 256) (HasExt True)
parseKeyVariety "BLAKE2B160"   = Blake2bKey (HashSize 160) (HasExt False)
parseKeyVariety "BLAKE2B160E"  = Blake2bKey (HashSize 160) (HasExt True)
parseKeyVariety "BLAKE2B224"   = Blake2bKey (HashSize 224) (HasExt False)
parseKeyVariety "BLAKE2B224E"  = Blake2bKey (HashSize 224) (HasExt True)
parseKeyVariety "BLAKE2B256"   = Blake2bKey (HashSize 256) (HasExt False)
parseKeyVariety "BLAKE2B256E"  = Blake2bKey (HashSize 256) (HasExt True)
parseKeyVariety "BLAKE2B384"   = Blake2bKey (HashSize 384) (HasExt False)
parseKeyVariety "BLAKE2B384E"  = Blake2bKey (HashSize 384) (HasExt True)
parseKeyVariety "BLAKE2B512"   = Blake2bKey (HashSize 512) (HasExt False)
parseKeyVariety "BLAKE2B512E"  = Blake2bKey (HashSize 512) (HasExt True)
parseKeyVariety "BLAKE2BP512"  = Blake2bpKey (HashSize 512) (HasExt False)
parseKeyVariety "BLAKE2BP512E" = Blake2bpKey (HashSize 512) (HasExt True)
parseKeyVariety "BLAKE2S160"   = Blake2sKey (HashSize 160) (HasExt False)
parseKeyVariety "BLAKE2S160E"  = Blake2sKey (HashSize 160) (HasExt True)
parseKeyVariety "BLAKE2S224"   = Blake2sKey (HashSize 224) (HasExt False)
parseKeyVariety "BLAKE2S224E"  = Blake2sKey (HashSize 224) (HasExt True)
parseKeyVariety "BLAKE2S256"   = Blake2sKey (HashSize 256) (HasExt False)
parseKeyVariety "BLAKE2S256E"  = Blake2sKey (HashSize 256) (HasExt True)
parseKeyVariety "BLAKE2SP224"  = Blake2spKey (HashSize 224) (HasExt False)
parseKeyVariety "BLAKE2SP224E" = Blake2spKey (HashSize 224) (HasExt True)
parseKeyVariety "BLAKE2SP256"  = Blake2spKey (HashSize 256) (HasExt False)
parseKeyVariety "BLAKE2SP256E" = Blake2spKey (HashSize 256) (HasExt True)
parseKeyVariety "SHA1"         = SHA1Key (HasExt False)
parseKeyVariety "SHA1E"        = SHA1Key (HasExt True)
parseKeyVariety "MD5"          = MD5Key (HasExt False)
parseKeyVariety "MD5E"         = MD5Key (HasExt True)
parseKeyVariety "WORM"         = WORMKey
parseKeyVariety "URL"          = URLKey
parseKeyVariety b
	| "X" `S.isPrefixOf` b = 
		let b' = S.tail b
		in if not (S.null b') && S.last b' == fromIntegral (ord 'E')
			then ExternalKey (S.init b') (HasExt True)
			else ExternalKey b' (HasExt False)
	| otherwise = OtherKey b

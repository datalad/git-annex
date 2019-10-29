{- git-annex Key data type
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Types.Key where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import System.Posix.Types
import Data.Monoid
import GHC.Generics
import Control.DeepSeq
import Prelude

{- A Key has a unique name, which is derived from a particular backend,
 - and may contain other optional metadata. -}
data Key = Key
	{ keyName :: S.ByteString
	, keyVariety :: KeyVariety
	, keySize :: Maybe Integer
	, keyMtime :: Maybe EpochTime
	, keyChunkSize :: Maybe Integer
	, keyChunkNum :: Maybe Integer
	} deriving (Eq, Ord, Read, Show, Generic)

instance NFData Key

{- A filename may be associated with a Key. -}
newtype AssociatedFile = AssociatedFile (Maybe FilePath)
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

{- Is the Key variety cryptographically secure, such that no two differing
 - file contents can be mapped to the same Key? -}
cryptographicallySecure :: KeyVariety -> Bool
cryptographicallySecure (SHA2Key _ _) = True
cryptographicallySecure (SHA3Key _ _) = True
cryptographicallySecure (SKEINKey _ _) = True
cryptographicallySecure (Blake2bKey _ _) = True
cryptographicallySecure (Blake2bpKey _ _) = True
cryptographicallySecure (Blake2sKey _ _) = True
cryptographicallySecure (Blake2spKey _ _) = True
cryptographicallySecure _ = False

{- Is the Key variety backed by a hash, which allows verifying content?
 - It does not have to be cryptographically secure against eg birthday
 - attacks.
 -}
isVerifiable :: KeyVariety -> Bool
isVerifiable (SHA2Key _ _) = True
isVerifiable (SHA3Key _ _) = True
isVerifiable (SKEINKey _ _) = True
isVerifiable (Blake2bKey _ _) = True
isVerifiable (Blake2bpKey _ _) = True
isVerifiable (Blake2sKey _ _) = True
isVerifiable (Blake2spKey _ _) = True
isVerifiable (SHA1Key _) = True
isVerifiable (MD5Key _) = True
isVerifiable WORMKey = False
isVerifiable URLKey = False
isVerifiable (OtherKey  _) =  False

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
parseKeyVariety "SHA1"        = SHA1Key (HasExt False)
parseKeyVariety "SHA1E"       = SHA1Key (HasExt True)
parseKeyVariety "MD5"         = MD5Key (HasExt False)
parseKeyVariety "MD5E"        = MD5Key (HasExt True)
parseKeyVariety "WORM"        = WORMKey
parseKeyVariety "URL"         = URLKey
parseKeyVariety b             = OtherKey b

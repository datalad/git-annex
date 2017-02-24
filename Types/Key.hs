{- git-annex Key data type
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Key where

import Utility.PartialPrelude

import System.Posix.Types

{- A Key has a unique name, which is derived from a particular backend,
 - and may contain other optional metadata. -}
data Key = Key
	{ keyName :: String
	, keyVariety :: KeyVariety
	, keySize :: Maybe Integer
	, keyMtime :: Maybe EpochTime
	, keyChunkSize :: Maybe Integer
	, keyChunkNum :: Maybe Integer
	} deriving (Eq, Ord, Read, Show)

{- A filename may be associated with a Key. -}
type AssociatedFile = Maybe FilePath

{- There are several different varieties of keys. -}
data KeyVariety
	= SHA2Key HashSize HasExt
	| SHA3Key HashSize HasExt
	| SKEINKey HashSize HasExt
	| SHA1Key HasExt
	| MD5Key HasExt
	| WORMKey
	| URLKey
 	-- Some repositories may contain keys of other varieties,
	-- which can still be processed to some extent.
	| OtherKey String
	deriving (Eq, Ord, Read, Show)

{- Some varieties of keys may contain an extension at the end of the
 - keyName -}
newtype HasExt = HasExt Bool
	deriving (Eq, Ord, Read, Show)

newtype HashSize = HashSize Int
	deriving (Eq, Ord, Read, Show)

hasExt :: KeyVariety -> Bool
hasExt (SHA2Key _ (HasExt b)) = b
hasExt (SHA3Key _ (HasExt b)) = b
hasExt (SKEINKey _ (HasExt b)) = b
hasExt (SHA1Key (HasExt b)) = b
hasExt (MD5Key (HasExt b)) = b
hasExt WORMKey = False
hasExt URLKey = False
hasExt (OtherKey s) =  end s == "E"

sameExceptExt :: KeyVariety -> KeyVariety -> Bool
sameExceptExt (SHA2Key sz1 _) (SHA2Key sz2 _) = sz1 == sz2
sameExceptExt (SHA3Key sz1 _) (SHA3Key sz2 _) = sz1 == sz2
sameExceptExt (SKEINKey sz1 _) (SKEINKey sz2 _) = sz1 == sz2
sameExceptExt (SHA1Key _) (SHA1Key _) = True
sameExceptExt (MD5Key _) (MD5Key _) = True
sameExceptExt _ _ = False

formatKeyVariety :: KeyVariety -> String
formatKeyVariety v = case v of
	SHA2Key sz e -> adde e (addsz sz "SHA")
	SHA3Key sz e -> adde e (addsz sz "SHA3_")
	SKEINKey sz e -> adde e (addsz sz "SKEIN")
	SHA1Key e -> adde e "SHA1"
	MD5Key e -> adde e "MD5"
	WORMKey -> "WORM"
	URLKey -> "URL"
	OtherKey s -> s
  where
	adde (HasExt False) s = s
	adde (HasExt True) s = s ++ "E"
	addsz (HashSize n) s =  s ++ show n

parseKeyVariety :: String -> KeyVariety
parseKeyVariety "SHA256"    =  SHA2Key (HashSize 256) (HasExt False)
parseKeyVariety "SHA256E"   = SHA2Key (HashSize 256) (HasExt True)
parseKeyVariety "SHA512"    = SHA2Key (HashSize 512) (HasExt False)
parseKeyVariety "SHA512E"   = SHA2Key (HashSize 512) (HasExt True)
parseKeyVariety "SHA224"    = SHA2Key (HashSize 224) (HasExt False)
parseKeyVariety "SHA224E"   = SHA2Key (HashSize 224) (HasExt True)
parseKeyVariety "SHA384"    = SHA2Key (HashSize 384) (HasExt False)
parseKeyVariety "SHA384E"   = SHA2Key (HashSize 384) (HasExt True)
parseKeyVariety "SHA3_512"  = SHA3Key (HashSize 512) (HasExt False)
parseKeyVariety "SHA3_512E" = SHA3Key (HashSize 512) (HasExt True)
parseKeyVariety "SHA3_384"  = SHA3Key (HashSize 384) (HasExt False)
parseKeyVariety "SHA3_384E" = SHA3Key (HashSize 384) (HasExt True)
parseKeyVariety "SHA3_256"  = SHA3Key (HashSize 256) (HasExt False)
parseKeyVariety "SHA3_256E" = SHA3Key (HashSize 256) (HasExt True)
parseKeyVariety "SHA3_224"  = SHA3Key (HashSize 224) (HasExt False)
parseKeyVariety "SHA3_224E" = SHA3Key (HashSize 224) (HasExt True)
parseKeyVariety "SKEIN512"  = SKEINKey (HashSize 512) (HasExt False)
parseKeyVariety "SKEIN512E" = SKEINKey (HashSize 512) (HasExt True)
parseKeyVariety "SKEIN256"  = SKEINKey (HashSize 256) (HasExt False)
parseKeyVariety "SKEIN256E" = SKEINKey (HashSize 256) (HasExt True)
parseKeyVariety "SHA1"      = SHA1Key (HasExt False)
parseKeyVariety "MD5"       = MD5Key (HasExt False)
parseKeyVariety "WORM"      = WORMKey
parseKeyVariety "URL"       = URLKey
parseKeyVariety s           = OtherKey s

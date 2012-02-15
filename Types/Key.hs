{- git-annex Key data type
 - 
 - Most things should not need this, using Types instead
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Key (
	Key(..),
	stubKey,
	readKey,

	prop_idempotent_key_read_show
) where

import System.Posix.Types

import Common

{- A Key has a unique name, is associated with a key/value backend,
 - and may contain other optional metadata. -}
data Key = Key {
	keyName :: String,
	keyBackendName :: String,
	keySize :: Maybe Integer,
	keyMtime :: Maybe EpochTime
} deriving (Eq, Ord)

stubKey :: Key
stubKey = Key {
	keyName = "",
	keyBackendName = "",
	keySize = Nothing,
	keyMtime = Nothing
}

fieldSep :: Char
fieldSep = '-'

{- Keys show as strings that are suitable for use as filenames.
 - The name field is always shown last, separated by doubled fieldSeps,
 - and is the only field allowed to contain the fieldSep. -}
instance Show Key where
	show Key { keyBackendName = b, keySize = s, keyMtime = m, keyName = n } =
		b +++ ('s' ?: s) +++ ('m' ?: m) +++ (fieldSep : n)
		where
			"" +++ y = y
			x +++ "" = x
			x +++ y = x ++ fieldSep:y
			c ?: (Just v) = c : show v
			_ ?: _ = ""

readKey :: String -> Maybe Key
readKey s = if key == Just stubKey then Nothing else key
	where
		key = startbackend stubKey s

		startbackend k v = sepfield k v addbackend
		
		sepfield k v a = case span (/= fieldSep) v of
			(v', _:r) -> findfields r $ a k v'
			_ -> Nothing

		findfields (c:v) (Just k)
			| c == fieldSep = Just $ k { keyName = v }
			| otherwise = sepfield k v $ addfield c
		findfields _ v = v

		addbackend k v = Just k { keyBackendName = v }
		addfield 's' k v = Just k { keySize = readish v }
		addfield 'm' k v = Just k { keyMtime = readish v }
		addfield _ _ _ = Nothing

prop_idempotent_key_read_show :: Key -> Bool
prop_idempotent_key_read_show k = Just k == (readKey . show) k

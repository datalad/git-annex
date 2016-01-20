{- git-annex backend utilities
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.Utilities where

import Data.Hash.MD5

import Annex.Common

{- Generates a keyName from an input string. Takes care of sanitizing it.
 - If it's not too long, the full string is used as the keyName.
 - Otherwise, it's truncated, and its md5 is prepended to ensure a unique
 - key. -}
genKeyName :: String -> String
genKeyName s
	-- Avoid making keys longer than the length of a SHA256 checksum.
	| bytelen > sha256len =
		truncateFilePath (sha256len - md5len - 1) s' ++ "-" ++ md5s (Str s)
	| otherwise = s'
  where
	s' = preSanitizeKeyName s
	bytelen = length (decodeW8 s')

	sha256len = 64
	md5len = 32


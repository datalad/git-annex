{- git-annex backend utilities
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.Utilities where

import Annex.Common
import Utility.Hash

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

{- Generates a keyName from an input string. Takes care of sanitizing it.
 - If it's not too long, the full string is used as the keyName.
 - Otherwise, it's truncated, and its md5 is prepended to ensure a unique
 - key. -}
genKeyName :: String -> S.ByteString
genKeyName s
	-- Avoid making keys longer than the length of a SHA256 checksum.
	| bytelen > sha256len = encodeBS' $
		truncateFilePath (sha256len - md5len - 1) s' ++ "-" ++ 
			show (md5 bl)
	| otherwise = encodeBS' s'
  where
	s' = preSanitizeKeyName s
	bl = encodeBL s
	bytelen = fromIntegral $ L.length bl

	sha256len = 64
	md5len = 32


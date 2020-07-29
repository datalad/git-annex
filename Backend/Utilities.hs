{- git-annex backend utilities
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Backend.Utilities where

import Annex.Common
import qualified Annex
import Utility.Hash
import Types.Key
import Types.KeySource

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P
import Data.Char
import Data.Word

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

{- Converts a key to a version that includes an extension from the
 - file that the key was generated from.  -}
addE :: KeySource -> (KeyVariety -> KeyVariety) -> Key -> Annex Key
addE source sethasext k = do
	maxlen <- annexMaxExtensionLength <$> Annex.getGitConfig
	let ext = selectExtension maxlen (keyFilename source)
	return $ alterKey k $ \d -> d
		{ keyName = keyName d <> ext
		, keyVariety = sethasext (keyVariety d)
		}

selectExtension :: Maybe Int -> RawFilePath -> S.ByteString
selectExtension maxlen f
	| null es = ""
	| otherwise = S.intercalate "." ("":es)
  where
	es = filter (not . S.null) $ reverse $
		take 2 $ filter (S.all validInExtension) $
		takeWhile shortenough $
		reverse $ S.split (fromIntegral (ord '.')) (P.takeExtensions f)
	shortenough e = S.length e <= fromMaybe maxExtensionLen maxlen

validInExtension :: Word8 -> Bool
validInExtension c
	| isAlphaNum (chr (fromIntegral c)) = True
	| fromIntegral c == ord '.' = True
	| c <= 127 = False -- other ascii: spaces, punctuation, control chars
	| otherwise = True -- utf8 is allowed, also other encodings

maxExtensionLen :: Int
maxExtensionLen = 4 -- long enough for "jpeg"

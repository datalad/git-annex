{- git-annex backend utilities
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
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
import qualified Utility.OsString as OS

import qualified Data.ByteString as S
import qualified Data.ByteString.Short as S (ShortByteString, toShort)
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Word

{- Generates a keyName from an input string. Takes care of sanitizing it.
 - If it's not too long, the full string is used as the keyName.
 - Otherwise, it's truncated, and its md5 is prepended to ensure a unique
 - key. -}
genKeyName :: String -> S.ShortByteString
genKeyName s
	-- Avoid making keys longer than the length of a SHA256 checksum.
	| bytelen > sha256len = S.toShort $
		truncateFilePath (sha256len - md5len - 1) s' 
			<> "-" <> encodeBS (show (md5 bl))
	| otherwise = S.toShort s'
  where
	s' = encodeBS $ preSanitizeKeyName s
	bl = encodeBL s
	bytelen = fromIntegral $ L.length bl

	sha256len = 64
	md5len = 32

{- Converts a key to a version that includes an extension from the
 - file that the key was generated from.  -}
addE :: KeySource -> (KeyVariety -> KeyVariety) -> Key -> Annex Key
addE source sethasext k = do
	c <- Annex.getGitConfig
	let ext = selectExtension
		(annexMaxExtensionLength c)
		(annexMaxExtensions c)
		(keyFilename source)
	return $ alterKey k $ \d -> d
		{ keyName = keyName d <> S.toShort ext
		, keyVariety = sethasext (keyVariety d)
		}

selectExtension :: Maybe Int -> Maybe Int -> OsPath -> S.ByteString
selectExtension maxlen maxextensions f
	| null es = ""
	| otherwise = S.intercalate "." ("":es)
  where
	es = filter (not . S.null) $ reverse $
		take (fromMaybe maxExtensions maxextensions) $
		filter (S.all validInExtension) $
		takeWhile shortenough $
		reverse $ S.split (fromIntegral (ord '.')) $
			fromOsPath $ takeExtensions f'
	shortenough e = S.length e <= fromMaybe maxExtensionLen maxlen
	-- Avoid treating a file ".foo" as having its whole name as an
	-- extension.
	f' = OS.dropWhile (== unsafeFromChar '.') (takeFileName f)

validInExtension :: Word8 -> Bool
validInExtension c
	| isAlphaNum (chr (fromIntegral c)) = True
	| fromIntegral c == ord '.' = True
	| c <= 127 = False -- other ascii: spaces, punctuation, control chars
	| otherwise = True -- utf8 is allowed, also other encodings

maxExtensionLen :: Int
maxExtensionLen = 4 -- long enough for "jpeg"

maxExtensions :: Int
maxExtensions = 2 -- include both extensions of "tar.gz"

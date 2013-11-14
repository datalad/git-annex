{- git-annex crypto types
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Crypto (
	Cipher(..),
	StorableCipher(..),
	EncryptedCipherVariant(..),
	KeyIds(..),
	Mac(..),
	readMac,
	showMac,
	defaultMac,
	calcMac,
) where

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.SHA

import Utility.Gpg (KeyIds(..))

-- XXX ideally, this would be a locked memory region
data Cipher = Cipher String | MacOnlyCipher String

data StorableCipher = EncryptedCipher String EncryptedCipherVariant KeyIds
		| SharedCipher String
	deriving (Ord, Eq)
data EncryptedCipherVariant = Hybrid | PubKey
	deriving (Ord, Eq)

{- File names are (client-side) MAC'ed on special remotes.
 - The chosen MAC algorithm needs to be same for all files stored on the
 - remote.
 -}
data Mac = HmacSha1 | HmacSha224 | HmacSha256 | HmacSha384 | HmacSha512
	deriving (Eq)

defaultMac :: Mac
defaultMac = HmacSha1

-- MAC algorithms are shown as follows in the file names.
showMac :: Mac -> String
showMac HmacSha1   = "HMACSHA1"
showMac HmacSha224 = "HMACSHA224"
showMac HmacSha256 = "HMACSHA256"
showMac HmacSha384 = "HMACSHA384"
showMac HmacSha512 = "HMACSHA512"

-- Read the MAC algorithm from the remote config.
readMac :: String -> Maybe Mac
readMac "HMACSHA1"   = Just HmacSha1
readMac "HMACSHA224" = Just HmacSha224
readMac "HMACSHA256" = Just HmacSha256
readMac "HMACSHA384" = Just HmacSha384
readMac "HMACSHA512" = Just HmacSha512
readMac _ = Nothing

calcMac
  :: Mac	  -- ^ MAC
  -> L.ByteString -- ^ secret key
  -> L.ByteString -- ^ message
  -> String	  -- ^ MAC'ed message, in hexadecimals
calcMac mac = case mac of
	HmacSha1   -> showDigest $* hmacSha1
	HmacSha224 -> showDigest $* hmacSha224
	HmacSha256 -> showDigest $* hmacSha256
	HmacSha384 -> showDigest $* hmacSha384
	HmacSha512 -> showDigest $* hmacSha512
  where
	($*) g f x y = g $ f x y

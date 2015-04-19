{- git-annex crypto types
 -
 - Copyright 2011-2012 Joey Hess <id@joeyh.name>
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
	:: Mac          -- ^ MAC
	-> L.ByteString -- ^ secret key
	-> L.ByteString -- ^ message
	-> String       -- ^ MAC'ed message, in hexadecimal
calcMac mac = case mac of
	HmacSha1   -> showDigest $* hmacSha1
	HmacSha224 -> showDigest $* hmacSha224
	HmacSha256 -> showDigest $* hmacSha256
	HmacSha384 -> showDigest $* hmacSha384
	HmacSha512 -> showDigest $* hmacSha512
  where
	($*) g f x y = g $ f x y

-- Check that all the MACs continue to produce the same.
prop_mac_stable :: Bool
prop_mac_stable = all (\(mac, result) -> calcMac mac key msg == result)
	[ (HmacSha1, "46b4ec586117154dacd49d664e5d63fdc88efb51")
	, (HmacSha224, "4c1f774863acb63b7f6e9daa9b5c543fa0d5eccf61e3ffc3698eacdd")
	, (HmacSha256, "f9320baf0249169e73850cd6156ded0106e2bb6ad8cab01b7bbbebe6d1065317")
	, (HmacSha384, "3d10d391bee2364df2c55cf605759373e1b5a4ca9355d8f3fe42970471eca2e422a79271a0e857a69923839015877fc6")
	, (HmacSha512, "114682914c5d017dfe59fdc804118b56a3a652a0b8870759cf9e792ed7426b08197076bf7d01640b1b0684df79e4b67e37485669e8ce98dbab60445f0db94fce")
	]
  where
	key = L.fromChunks [T.encodeUtf8 $ T.pack "foo"]
	msg = L.fromChunks [T.encodeUtf8 $ T.pack "bar"]

{- git-annex crypto types
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Crypto (
	EncryptionMethod(..),
	Cipher(..),
	StorableCipher(..),
	EncryptedCipherVariant(..),
	KeyIds(..),
	cipherKeyIds,
	Mac(..),
	readMac,
	showMac,
	defaultMac,
	calcMac,
) where

import Utility.Hash
import Utility.Gpg (KeyIds(..))

import Data.Typeable

data EncryptionMethod
	= NoneEncryption
	| SharedEncryption
	| PubKeyEncryption
	| SharedPubKeyEncryption
	| HybridEncryption
	deriving (Typeable, Eq)

-- XXX ideally, this would be a locked memory region
data Cipher = Cipher String | MacOnlyCipher String

data StorableCipher
	= EncryptedCipher String EncryptedCipherVariant KeyIds
	| SharedCipher String
	| SharedPubKeyCipher String KeyIds
	deriving (Ord, Eq)
data EncryptedCipherVariant = Hybrid | PubKey
	deriving (Ord, Eq)

cipherKeyIds :: StorableCipher -> Maybe KeyIds
cipherKeyIds (EncryptedCipher _ _ ks) = Just ks
cipherKeyIds (SharedPubKeyCipher _ ks) = Just ks
cipherKeyIds (SharedCipher _) = Nothing

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

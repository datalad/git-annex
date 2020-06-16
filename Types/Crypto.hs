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
	macMap,
	defaultMac,
	calcMac,
) where

import Utility.Hash
import Utility.Gpg (KeyIds(..))

import Data.Typeable
import qualified Data.Map as M

data EncryptionMethod
	= NoneEncryption
	| SharedEncryption
	| PubKeyEncryption
	| SharedPubKeyEncryption
	| HybridEncryption
	deriving (Typeable, Eq)

-- XXX ideally, this would be a locked memory region
data Cipher = Cipher String | MacOnlyCipher String
	deriving (Show) -- XXXDO NOT COMMIT

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
readMac n = M.lookup n macMap

macMap :: M.Map String Mac
macMap = M.fromList
	[ ("HMACSHA1", HmacSha1)
	, ("HMACSHA224", HmacSha224)
	, ("HMACSHA256", HmacSha256)
	, ("HMACSHA384", HmacSha384)
	, ("HMACSHA512", HmacSha512)
	]

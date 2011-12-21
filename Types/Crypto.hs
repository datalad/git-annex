{- git-annex crypto types
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Crypto (
	Cipher(..),
	EncryptedCipher(..),
	KeyIds(..),
) where

import Utility.Gpg (KeyIds(..))

-- XXX ideally, this would be a locked memory region
newtype Cipher = Cipher String

data EncryptedCipher = EncryptedCipher String KeyIds
	deriving (Ord, Eq)

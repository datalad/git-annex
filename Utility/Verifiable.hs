{- values verified using a shared secret
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Verifiable where

import Data.Digest.Pure.SHA
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy as L

type Secret = L.ByteString
type HMACDigest = String

{- A value, verifiable using a HMAC digest and a secret. -}
data Verifiable a = Verifiable
	{ verifiableVal :: a
	, verifiableDigest :: HMACDigest
	}
	deriving (Eq, Read, Show)

mkVerifiable :: Show a => a -> Secret -> Verifiable a
mkVerifiable a secret = Verifiable a (calcDigest (show a) secret)

verify :: (Eq a, Show a) => Verifiable a -> Secret -> Bool
verify v secret = v == mkVerifiable (verifiableVal v) secret

calcDigest :: String -> Secret -> HMACDigest
calcDigest v secret = showDigest $ hmacSha1 secret $ fromString v

{- for quickcheck -}
prop_verifiable_sane :: String -> String -> Bool
prop_verifiable_sane a s = verify (mkVerifiable a secret) secret
  where
	secret = fromString s

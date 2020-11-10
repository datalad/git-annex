{- values verified using a shared secret
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Verifiable (
	Secret,
	HMACDigest,
	Verifiable(..),
	mkVerifiable,
	verify,
	prop_verifiable_sane,
) where

import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as S

import Utility.Hash
import Utility.QuickCheck

type Secret = S.ByteString
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
calcDigest v secret = calcMac HmacSha1 secret (fromString v)

prop_verifiable_sane :: TestableString -> TestableString -> Bool
prop_verifiable_sane v ts = 
	verify (mkVerifiable (fromTestableString v) secret) secret
  where
	secret = fromString (fromTestableString ts)

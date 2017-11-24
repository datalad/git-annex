{- authentication tokens
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.AuthToken (
	AuthToken,
	toAuthToken,
	fromAuthToken,
	nullAuthToken,
	genAuthToken,
	AllowedAuthTokens,
	allowedAuthTokens,
	isAllowedAuthToken,
) where

import qualified Utility.SimpleProtocol as Proto
import Utility.Hash

import Data.SecureMem
import Data.Maybe
import Data.Char
import Data.Byteable
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as L
import "crypto-api" Crypto.Random

-- | An AuthToken is stored in secue memory, with constant time comparison.
--
-- It can have varying length, depending on the security needs of the
-- application.
--
-- To avoid decoding issues, and presentation issues, the content
-- of an AuthToken is limited to ASCII characters a-z, and 0-9.
-- This is enforced by all exported AuthToken constructors.
newtype AuthToken = AuthToken SecureMem
	deriving (Show, Eq)

allowedChar :: Char -> Bool
allowedChar c = isAsciiUpper c || isAsciiLower c || isDigit c

instance Proto.Serializable AuthToken where
	serialize = T.unpack . fromAuthToken
	deserialize = toAuthToken . T.pack

fromAuthToken :: AuthToken -> T.Text
fromAuthToken (AuthToken t ) = TE.decodeLatin1 (toBytes t)

-- | Upper-case characters are lower-cased to make them fit in the allowed
-- character set. This allows AuthTokens to be compared effectively
-- case-insensitively. 
--
-- Returns Nothing if any disallowed characters are present.
toAuthToken :: T.Text -> Maybe AuthToken
toAuthToken t
	| all allowedChar s = Just $ AuthToken $ 
		secureMemFromByteString $ TE.encodeUtf8 $ T.pack s
	| otherwise = Nothing
  where
	s = map toLower $ T.unpack t

-- | The empty AuthToken, for those times when you don't want any security.
nullAuthToken :: AuthToken
nullAuthToken = AuthToken $ secureMemFromByteString $ TE.encodeUtf8 T.empty

-- | Generates an AuthToken of a specified length. This is done by
-- generating a random bytestring, hashing it with sha2 512, and truncating
-- to the specified length.
--
-- That limits the maximum length to 128, but with 512 bytes of entropy,
-- that should be sufficient for any application.
genAuthToken :: Int -> IO AuthToken
genAuthToken len = do
	g <- newGenIO :: IO SystemRandom
	return $
		case genBytes 512 g of
			Left e -> error $ "failed to generate auth token: " ++ show e
			Right (s, _) -> fromMaybe (error "auth token encoding failed") $
				toAuthToken $ T.pack $ take len $
					show $ sha2_512 $ L.fromChunks [s]

-- | For when several AuthTokens are allowed to be used.
newtype AllowedAuthTokens = AllowedAuthTokens [AuthToken]

allowedAuthTokens :: [AuthToken] -> AllowedAuthTokens
allowedAuthTokens = AllowedAuthTokens

-- | Note that every item in the list is checked, even if the first one
-- is allowed, so that comparison is constant-time.
isAllowedAuthToken :: AuthToken -> AllowedAuthTokens -> Bool
isAllowedAuthToken t (AllowedAuthTokens l) = go False l
  where
	go ok [] = ok
	go ok (i:is)
		| t == i = go True is
		| otherwise = go ok is

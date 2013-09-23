{- Convenience wrapper around cryptohash.
 -
 - The resulting Digests can be shown to get a canonical hash encoding. -}

module Utility.Hash where

import Crypto.Hash
import qualified Data.ByteString.Lazy as L

sha1 :: L.ByteString -> Digest SHA1
sha1 = hashlazy

sha224 :: L.ByteString -> Digest SHA224
sha224 = hashlazy

sha256 :: L.ByteString -> Digest SHA256
sha256 = hashlazy

sha384 :: L.ByteString -> Digest SHA384
sha384 = hashlazy

sha512 :: L.ByteString -> Digest SHA512
sha512 = hashlazy

-- sha3 is not yet fully standardized
--sha3 :: L.ByteString -> Digest SHA3
--sha3 = hashlazy



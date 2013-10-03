{- Convenience wrapper around cryptohash.
 - Falls back to SHA if it's not available.
 -}

{-# LANGUAGE CPP #-}

module Utility.Hash (
	sha1,
	sha224,
	sha256,
	sha384,
	sha512,
#ifdef WITH_CRYPTOHASH
	skein256,
	skein512,
#endif
	prop_hashes_stable
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8

#ifndef WITH_CRYPTOHASH
import Data.Digest.Pure.SHA
#else
import Crypto.Hash

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

skein256 :: L.ByteString -> Digest Skein256_256
skein256 = hashlazy

skein512 :: L.ByteString -> Digest Skein512_512
skein512 = hashlazy

#endif

{- Check that all the hashes continue to hash the same. -}
prop_hashes_stable :: Bool
prop_hashes_stable = all (\(hasher, result) -> hasher foo == result)
	[ (show . sha1, "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33")
	, (show . sha224, "0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db")
	, (show . sha256, "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae")
	, (show . sha384, "98c11ffdfdd540676b1a137cb1a22b2a70350c9a44171d6b1180c6be5cbb2ee3f79d532c8a1dd9ef2e8e08e752a3babb")
	, (show . sha512, "f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7")
#ifdef WITH_CRYPTOHASH
	, (show . skein256, "a04efd9a0aeed6ede40fe5ce0d9361ae7b7d88b524aa19917b9315f1ecf00d33")
	, (show . skein512, "fd8956898113510180aa4658e6c0ac85bd74fb47f4a4ba264a6b705d7a8e8526756e75aecda12cff4f1aca1a4c2830fbf57f458012a66b2b15a3dd7d251690a7")
#endif
	]
  where
	foo = L.fromChunks [C8.pack "foo"]

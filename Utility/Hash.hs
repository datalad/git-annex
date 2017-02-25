{- Convenience wrapper around cryptohash/cryptonite.
 -
 - SHA3 hashes are currently only enabled when using cryptonite,
 - because of https://github.com/vincenthz/hs-cryptohash/issues/36
 -}

module Utility.Hash (
	sha1,
	sha2_224,
	sha2_256,
	sha2_384,
	sha2_512,
	sha3_224,
	sha3_256,
	sha3_384,
	sha3_512,
	skein256,
	skein512,
	md5,
	prop_hashes_stable,
	Mac(..),
	calcMac,
	prop_mac_stable,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import "cryptonite" Crypto.MAC.HMAC
import "cryptonite" Crypto.Hash

sha1 :: L.ByteString -> Digest SHA1
sha1 = hashlazy

sha2_224 :: L.ByteString -> Digest SHA224
sha2_224 = hashlazy

sha2_256 :: L.ByteString -> Digest SHA256
sha2_256 = hashlazy

sha2_384 :: L.ByteString -> Digest SHA384
sha2_384 = hashlazy

sha2_512 :: L.ByteString -> Digest SHA512
sha2_512 = hashlazy

sha3_224 :: L.ByteString -> Digest SHA3_224
sha3_224 = hashlazy

sha3_256 :: L.ByteString -> Digest SHA3_256
sha3_256 = hashlazy

sha3_384 :: L.ByteString -> Digest SHA3_384
sha3_384 = hashlazy

sha3_512 :: L.ByteString -> Digest SHA3_512
sha3_512 = hashlazy

skein256 :: L.ByteString -> Digest Skein256_256
skein256 = hashlazy

skein512 :: L.ByteString -> Digest Skein512_512
skein512 = hashlazy

md5 ::  L.ByteString -> Digest MD5
md5 = hashlazy

{- Check that all the hashes continue to hash the same. -}
prop_hashes_stable :: Bool
prop_hashes_stable = all (\(hasher, result) -> hasher foo == result)
	[ (show . sha1, "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33")
	, (show . sha2_224, "0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db")
	, (show . sha2_256, "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae")
	, (show . sha2_384, "98c11ffdfdd540676b1a137cb1a22b2a70350c9a44171d6b1180c6be5cbb2ee3f79d532c8a1dd9ef2e8e08e752a3babb")
	, (show . sha2_512, "f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7")
	, (show . skein256, "a04efd9a0aeed6ede40fe5ce0d9361ae7b7d88b524aa19917b9315f1ecf00d33")
	, (show . skein512, "fd8956898113510180aa4658e6c0ac85bd74fb47f4a4ba264a6b705d7a8e8526756e75aecda12cff4f1aca1a4c2830fbf57f458012a66b2b15a3dd7d251690a7")
	, (show . sha3_224, "f4f6779e153c391bbd29c95e72b0708e39d9166c7cea51d1f10ef58a")
	, (show . sha3_256, "76d3bc41c9f588f7fcd0d5bf4718f8f84b1c41b20882703100b9eb9413807c01")
	, (show . sha3_384, "665551928d13b7d84ee02734502b018d896a0fb87eed5adb4c87ba91bbd6489410e11b0fbcc06ed7d0ebad559e5d3bb5")
	, (show . sha3_512, "4bca2b137edc580fe50a88983ef860ebaca36c857b1f492839d6d7392452a63c82cbebc68e3b70a2a1480b4bb5d437a7cba6ecf9d89f9ff3ccd14cd6146ea7e7")
	, (show . md5, "acbd18db4cc2f85cedef654fccc4a4d8")
	]
  where
	foo = L.fromChunks [T.encodeUtf8 $ T.pack "foo"]

{- File names are (client-side) MAC'ed on special remotes.
 - The chosen MAC algorithm needs to be same for all files stored on the
 - remote.
 -}
data Mac = HmacSha1 | HmacSha224 | HmacSha256 | HmacSha384 | HmacSha512
	deriving (Eq)

calcMac
	:: Mac          -- ^ MAC
	-> S.ByteString -- ^ secret key
	-> S.ByteString -- ^ message
	-> String       -- ^ MAC'ed message, in hexadecimal
calcMac mac = case mac of
	HmacSha1   -> use SHA1
	HmacSha224 -> use SHA224
	HmacSha256 -> use SHA256
	HmacSha384 -> use SHA384
	HmacSha512 -> use SHA512
  where
	use alg k m = show (hmacGetDigest (hmacWitnessAlg alg k m))

	hmacWitnessAlg :: HashAlgorithm a => a -> S.ByteString -> S.ByteString -> HMAC a
	hmacWitnessAlg _ = hmac

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
	key = T.encodeUtf8 $ T.pack "foo"
	msg = T.encodeUtf8 $ T.pack "bar"

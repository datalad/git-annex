{- git-annex file locations
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.DirHashes (
	Hasher,
	HashLevels(..),
	objectHashLevels,
	branchHashLevels,
	branchHashDir,
	dirHashes,
	hashDirMixed,
	hashDirLower,
	display_32bits_as_dir
) where

import Data.Default
import Data.Bits
import qualified Data.ByteArray

import Common
import Key
import Types.GitConfig
import Types.Difference
import Utility.Hash
import Utility.MD5

type Hasher = Key -> FilePath

-- Number of hash levels to use. 2 is the default.
newtype HashLevels = HashLevels Int

instance Default HashLevels where
	def = HashLevels 2

objectHashLevels :: GitConfig -> HashLevels
objectHashLevels = configHashLevels OneLevelObjectHash

branchHashLevels :: GitConfig -> HashLevels
branchHashLevels = configHashLevels OneLevelBranchHash

configHashLevels :: Difference -> GitConfig -> HashLevels
configHashLevels d config
	| hasDifference d (annexDifferences config) = HashLevels 1
	| otherwise = def

branchHashDir :: GitConfig -> Key -> String
branchHashDir = hashDirLower . branchHashLevels

{- Two different directory hashes may be used. The mixed case hash
 - came first, and is fine, except for the problem of case-strict
 - filesystems such as Linux VFAT (mounted with shortname=mixed),
 - which do not allow using a directory "XX" when "xx" already exists.
 - To support that, some git-annex repositories use the lower case-hash.
 - All special remotes use the lower-case hash for new data, but old data
 - may still used the mixed case hash. -}
dirHashes :: [HashLevels -> Hasher]
dirHashes = [hashDirLower, hashDirMixed]

hashDirs :: HashLevels -> Int -> String -> FilePath
hashDirs (HashLevels 1) sz s = addTrailingPathSeparator $ take sz s
hashDirs _ sz s = addTrailingPathSeparator $ take sz s </> drop sz s

hashDirLower :: HashLevels -> Hasher
hashDirLower n k = hashDirs n 3 $ take 6 $ show $ md5 $ serializeKey' $ nonChunkKey k

{- This was originally using Data.Hash.MD5 from MissingH. This new version
- is faster, but ugly as it has to replicate the 4 Word32's that produced. -}
hashDirMixed :: HashLevels -> Hasher
hashDirMixed n k = hashDirs n 2 $ take 4 $ concatMap display_32bits_as_dir $
	encodeWord32 $ map fromIntegral $ Data.ByteArray.unpack $
		Utility.Hash.md5 $ serializeKey' $ nonChunkKey k
  where
	encodeWord32 (b1:b2:b3:b4:rest) =
		(shiftL b4 24 .|. shiftL b3 16 .|. shiftL b2 8 .|. b1)
		: encodeWord32 rest
	encodeWord32 _ = []

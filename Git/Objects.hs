{- .git/objects
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Objects where

import Common
import Git
import Git.Sha
import qualified Utility.OsString as OS

import qualified Data.ByteString as B
objectsDir :: Repo -> OsPath
objectsDir r = localGitDir r </> literalOsPath "objects"

packDir :: Repo -> OsPath
packDir r = objectsDir r </> literalOsPath "pack"

packIdxFile :: OsPath -> OsPath
packIdxFile = flip replaceExtension (literalOsPath "idx")

listPackFiles :: Repo -> IO [OsPath]
listPackFiles r = filter (literalOsPath ".pack" `OS.isSuffixOf`) 
	<$> catchDefaultIO [] (dirContents $ packDir r)

listLooseObjectShas :: Repo -> IO [Sha]
listLooseObjectShas r = catchDefaultIO [] $
	mapMaybe conv <$> emptyWhenDoesNotExist
		(dirContentsRecursiveSkipping ispackdir True (objectsDir r))
  where
	conv :: OsPath -> Maybe Sha
	conv = extractSha 
		. fromOsPath
		. OS.concat
		. reverse
		. take 2
		. reverse
		. splitDirectories
	ispackdir f = f == literalOsPath "pack"

looseObjectFile :: Repo -> Sha -> OsPath
looseObjectFile r sha = objectsDir r </> toOsPath prefix </> toOsPath rest
  where
	(prefix, rest) = B.splitAt 2 (fromRef' sha)

listAlternates :: Repo -> IO [FilePath]
listAlternates r = catchDefaultIO [] $
	lines <$> readFileString alternatesfile
  where
	alternatesfile = objectsDir r </> literalOsPath "info" </> literalOsPath "alternates"

{- A repository recently cloned with --shared will have one or more
 - alternates listed, and contain no loose objects or packs. -}
isSharedClone :: Repo -> IO Bool
isSharedClone r = allM id
	[ not . null <$> listAlternates r
	, null <$> listLooseObjectShas r
	, null <$> listPackFiles r
	]

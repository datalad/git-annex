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

import qualified Data.ByteString as B
import qualified System.FilePath.ByteString as P

objectsDir :: Repo -> RawFilePath
objectsDir r = localGitDir r P.</> "objects"

packDir :: Repo -> RawFilePath
packDir r = objectsDir r P.</> "pack"

packIdxFile :: RawFilePath -> RawFilePath
packIdxFile = flip P.replaceExtension "idx"

listPackFiles :: Repo -> IO [FilePath]
listPackFiles r = filter (".pack" `isSuffixOf`) 
	<$> catchDefaultIO [] (dirContents $ fromRawFilePath $ packDir r)

listLooseObjectShas :: Repo -> IO [Sha]
listLooseObjectShas r = catchDefaultIO [] $
	mapMaybe (extractSha . encodeBS . concat . reverse . take 2 . reverse . splitDirectories)
		<$> dirContentsRecursiveSkipping (== "pack") True (fromRawFilePath (objectsDir r))

looseObjectFile :: Repo -> Sha -> RawFilePath
looseObjectFile r sha = objectsDir r P.</> prefix P.</> rest
  where
	(prefix, rest) = B.splitAt 2 (fromRef' sha)

listAlternates :: Repo -> IO [FilePath]
listAlternates r = catchDefaultIO [] $
	lines <$> readFile (fromRawFilePath alternatesfile)
  where
	alternatesfile = objectsDir r P.</> "info" P.</> "alternates"

{- A repository recently cloned with --shared will have one or more
 - alternates listed, and contain no loose objects or packs. -}
isSharedClone :: Repo -> IO Bool
isSharedClone r = allM id
	[ not . null <$> listAlternates r
	, null <$> listLooseObjectShas r
	, null <$> listPackFiles r
	]

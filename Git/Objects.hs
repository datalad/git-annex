{- .git/objects
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Objects where

import Common
import Git
import Git.Sha

objectsDir :: Repo -> FilePath
objectsDir r = localGitDir r </> "objects"

packDir :: Repo -> FilePath
packDir r = objectsDir r </> "pack"

packIdxFile :: FilePath -> FilePath
packIdxFile = flip replaceExtension "idx"

listPackFiles :: Repo -> IO [FilePath]
listPackFiles r = filter (".pack" `isSuffixOf`) 
	<$> catchDefaultIO [] (dirContents $ packDir r)

listLooseObjectShas :: Repo -> IO [Sha]
listLooseObjectShas r = catchDefaultIO [] $
	mapMaybe (extractSha . concat . reverse . take 2 . reverse . splitDirectories)
		<$> dirContentsRecursiveSkipping (== "pack") True (objectsDir r)

looseObjectFile :: Repo -> Sha -> FilePath
looseObjectFile r sha = objectsDir r </> prefix </> rest
  where
	(prefix, rest) = splitAt 2 (fromRef sha)

listAlternates :: Repo -> IO [FilePath]
listAlternates r = catchDefaultIO [] (lines <$> readFile alternatesfile)
  where
	alternatesfile = objectsDir r </> "info" </> "alternates"

{- A repository recently cloned with --shared will have one or more
 - alternates listed, and contain no loose objects or packs. -}
isSharedClone :: Repo -> IO Bool
isSharedClone r = allM id
	[ not . null <$> listAlternates r
	, null <$> listLooseObjectShas r
	, null <$> listPackFiles r
	]

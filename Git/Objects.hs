{- .git/objects
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
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
		<$> dirContentsRecursiveSkipping (== "pack") (objectsDir r)

looseObjectFile :: Repo -> Sha -> FilePath
looseObjectFile r sha = objectsDir r </> prefix </> rest
  where
	(prefix, rest) = splitAt 2 (show sha)

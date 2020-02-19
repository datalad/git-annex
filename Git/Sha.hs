{- git SHA stuff
 -
 - Copyright 2011,2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Sha where

import Common
import Git.Types

{- Runs an action that causes a git subcommand to emit a Sha, and strips
 - any trailing newline, returning the sha. -}
getSha :: String -> IO String -> IO Sha
getSha subcommand a = maybe bad return =<< extractSha <$> a
  where
	bad = error $ "failed to read sha from git " ++ subcommand

{- Extracts the Sha from a string. There can be a trailing newline after
 - it, but nothing else. -}
extractSha :: String -> Maybe Sha
extractSha s
	| len `elem` shaSizes = val s
	| len - 1 `elem` shaSizes && length s' == len - 1 = val s'
	| otherwise = Nothing
  where
	len = length s
	s' = firstLine s
	val v
		| all (`elem` "1234567890ABCDEFabcdef") v = Just $ Ref v
		| otherwise = Nothing

{- Sizes of git shas. -}
shaSizes :: [Int]
shaSizes = 
	[ 40 -- sha1 (must come first)
	, 64 -- sha256
	]

{- Git plumbing often uses a all 0 sha to represent things like a
 - deleted file. -}
nullShas :: [Sha]
nullShas = map (\n -> Ref (replicate n '0')) shaSizes

{- Sha to provide to git plumbing when deleting a file.
 -
 - It's ok to provide a sha1; git versions that use sha256 will map the
 - sha1 to the sha256, or probably just treat all null sha1 specially
 - the same as all null sha256. -}
deleteSha :: Sha
deleteSha = Prelude.head nullShas

{- Git's magic empty tree.
 -
 - It's ok to provide the sha1 of this to git to refer to an empty tree;
 - git versions that use sha256 will map the sha1 to the sha256.
 -}
emptyTree :: Ref
emptyTree = Ref "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

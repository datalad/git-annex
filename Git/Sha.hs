{- git SHA stuff
 -
 - Copyright 2011,2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Sha where

import Common
import Git.Types

import qualified Data.ByteString as S
import Data.Char

{- Runs an action that causes a git subcommand to emit a Sha, and strips
 - any trailing newline, returning the sha. -}
getSha :: String -> IO S.ByteString -> IO Sha
getSha subcommand a = maybe bad return =<< extractSha <$> a
  where
	bad = error $ "failed to read sha from git " ++ subcommand

{- Extracts the Sha from a ByteString. 
 -
 - There can be a trailing newline after it, but nothing else.
 -}
extractSha :: S.ByteString -> Maybe Sha
extractSha s
	| len `elem` shaSizes = val s
	| len - 1 `elem` shaSizes && S.length s' == len - 1 = val s'
	| otherwise = Nothing
  where
	len = S.length s
	s' = firstLine' s
	val v
		| S.all validinsha v = Just $ Ref v
		| otherwise = Nothing
	validinsha w = or
		[ w >= 48 && w <= 57 -- 0-9
		, w >= 97 && w <= 102 -- a-f
		, w >= 65 && w <= 70 -- A-F
		]

{- Sizes of git shas. -}
shaSizes :: [Int]
shaSizes = 
	[ 40 -- sha1 (must come first)
	, 64 -- sha256
	]

{- Git plumbing often uses a all 0 sha to represent things like a
 - deleted file. -}
nullShas :: [Sha]
nullShas = map (\n -> Ref (S.replicate n zero)) shaSizes
  where
	zero = fromIntegral (ord '0')

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

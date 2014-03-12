{- git SHA stuff
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
	| len == shaSize = val s
	| len == shaSize + 1 && length s' == shaSize = val s'
	| otherwise = Nothing
  where
	len = length s
	s' = firstLine s
	val v
		| all (`elem` "1234567890ABCDEFabcdef") v = Just $ Ref v
		| otherwise = Nothing

{- Size of a git sha. -}
shaSize :: Int
shaSize = 40

nullSha :: Ref		
nullSha = Ref $ replicate shaSize '0'

{- Git's magic empty tree. -}
emptyTree :: Ref
emptyTree = Ref "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

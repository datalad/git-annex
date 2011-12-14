{- git SHA stuff
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Sha where

import Common
import Git.Types

{- Runs an action that causes a git subcommand to emit a sha, and strips
   any trailing newline, returning the sha. -}
getSha :: String -> IO String -> IO Sha
getSha subcommand a = do
	t <- a
	let t' = if last t == '\n'
		then init t
		else t
	when (length t' /= shaSize) $
		error $ "failed to read sha from git " ++ subcommand ++ " (" ++ t' ++ ")"
	return $ Ref t'

{- Size of a git sha. -}
shaSize :: Int
shaSize = 40

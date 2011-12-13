{- git hash-object interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.HashObject where

import Common
import Git

{- Injects a set of files into git, returning the shas of the objects. -}
hashFiles :: [FilePath] -> Repo -> IO [Sha]
hashFiles paths repo = do
	(pid, fromh, toh) <- hPipeBoth "git" $ toCommand $ git_hash_object repo
	_ <- forkProcess (feeder toh)
	hClose toh
	shas <- map Git.Ref . lines <$> hGetContents fromh
	hClose fromh
	forceSuccess pid
	return shas
	where
		git_hash_object = Git.gitCommandLine
			[Param "hash-object", Param "-w", Param "--stdin-paths"]
		feeder toh = do
			hPutStr toh $ unlines paths
			hClose toh
			exitSuccess

{- git hash-object interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.HashObject where

import Common
import Git
import Git.Command

{- Injects a set of files into git, returning the shas of the objects
 - and an IO action to call once the the shas have been used. -}
hashFiles :: [FilePath] -> Repo -> IO ([Sha], IO ())
hashFiles paths repo = do
	(pid, fromh, toh) <- hPipeBoth "git" $ toCommand $ git_hash_object repo
	_ <- forkProcess (feeder toh)
	hClose toh
	shas <- map Ref . lines <$> hGetContents fromh
	return (shas, ender fromh pid)
	where
		git_hash_object = gitCommandLine
			[Param "hash-object", Param "-w", Param "--stdin-paths"]
		feeder toh = do
			hPutStr toh $ unlines paths
			hClose toh
			exitSuccess
		ender fromh pid = do
			hClose fromh
			forceSuccess pid

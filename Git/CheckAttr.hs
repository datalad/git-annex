{- git check-attr interface
 -
 - Copyright 2010, 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CheckAttr where

import System.Exit

import Common
import Git
import Git.Command
import qualified Git.Filename
import qualified Git.Version

{- Efficiently looks up a gitattributes value for each file in a list. -}
lookup :: String -> [FilePath] -> Repo -> IO [(FilePath, String)]
lookup attr files repo = do
	cwd <- getCurrentDirectory
	(_, fromh, toh) <- hPipeBoth "git" (toCommand params)
        _ <- forkProcess $ do
		hClose fromh
		oldgit <- Git.Version.older "1.7.7"
                hPutStr toh $ join "\0" $ input cwd oldgit
                hClose toh
                exitSuccess
        hClose toh
	oldgit <- Git.Version.older "1.7.7"
	output cwd oldgit . lines <$> hGetContents fromh
	where
		params = gitCommandLine 
				[ Param "check-attr"
				, Param attr
				, Params "-z --stdin"
				] repo

		{- Before git 1.7.7, git check-attr worked best with
		 - absolute filenames; using them worked around some bugs
		 - with relative filenames.
		 - 
		 - With newer git, git check-attr chokes on some absolute
		 - filenames, and the bugs that necessitated them were fixed,
		 - so use relative filenames. -}
		input cwd oldgit
			| oldgit = map (absPathFrom cwd) files
			| otherwise = map (relPathDirToFile cwd . absPathFrom cwd) files
		output cwd oldgit
			| oldgit = map (torel cwd . topair)
			| otherwise = map topair

		topair l = (Git.Filename.decode file, value)
			where 
				file = join sep $ beginning bits
				value = end bits !! 0
				bits = split sep l
				sep = ": " ++ attr ++ ": "

		torel cwd (file, value) = (relfile, value)
			where
				relfile
					| startswith cwd' file = drop (length cwd') file
					| otherwise = relPathDirToFile top' file
				top = workTree repo
				cwd' = cwd ++ "/"
				top' = top ++ "/"

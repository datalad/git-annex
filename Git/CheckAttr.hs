{- git check-attr interface
 -
 - Copyright 2010, 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CheckAttr where

import Common
import Git
import Git.Command
import qualified Git.Version

{- Efficiently looks up a gitattributes value for each file in a list. -}
lookup :: String -> [FilePath] -> Repo -> IO [(FilePath, String)]
lookup attr files repo = do
	cwd <- getCurrentDirectory
	(_, fromh, toh) <- hPipeBoth "git" (toCommand params)
	hPutStr toh $ join "\0" $ input cwd
	hClose toh
	zip files . map attrvalue . lines <$> hGetContents fromh
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
		oldgit = Git.Version.older "1.7.7"
		input cwd
			| oldgit = map (absPathFrom cwd) files
			| otherwise = map (relPathDirToFile cwd . absPathFrom cwd) files
		attrvalue l = end bits !! 0
			where
				bits = split sep l
				sep = ": " ++ attr ++ ": "

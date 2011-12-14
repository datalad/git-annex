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

{- Efficiently looks up a gitattributes value for each file in a list. -}
lookup :: String -> [FilePath] -> Repo -> IO [(FilePath, String)]
lookup attr files repo = do
	-- git check-attr needs relative filenames input; it will choke
	-- on some absolute filenames. This also means it will output
	-- all relative filenames.
	cwd <- getCurrentDirectory
	let relfiles = map (relPathDirToFile cwd . absPathFrom cwd) files
	(_, fromh, toh) <- hPipeBoth "git" (toCommand params)
        _ <- forkProcess $ do
		hClose fromh
                hPutStr toh $ join "\0" relfiles
                hClose toh
                exitSuccess
        hClose toh
	(map topair . lines) <$> hGetContents fromh
	where
		params = gitCommandLine 
				[ Param "check-attr"
				, Param attr
				, Params "-z --stdin"
				] repo
		topair l = (file, value)
			where 
				file = Git.Filename.decode $ join sep $ take end bits
				value = bits !! end
				end = length bits - 1
				bits = split sep l
				sep = ": " ++ attr ++ ": "

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
import qualified Git.Filename

{- Efficiently looks up a gitattributes value for each file in a list. -}
lookup :: String -> [FilePath] -> Repo -> IO [(FilePath, String)]
lookup attr files repo = do
	-- this code is for git < 1.7, which changed git acheck-attr
	-- significantly!
	cwd <- getCurrentDirectory
	let top = workTree repo
	let absfiles = map (absPathFrom cwd) files
	(_, fromh, toh) <- hPipeBoth "git" (toCommand params)
        _ <- forkProcess $ do
		hClose fromh
                hPutStr toh $ join "\0" absfiles
                hClose toh
                exitSuccess
        hClose toh
	(map (topair cwd top) . lines) <$> hGetContents fromh
	where
		params = gitCommandLine 
				[ Param "check-attr"
				, Param attr
				, Params "-z --stdin"
				] repo
		topair cwd top l = (relfile, value)
			where 
				relfile
					| startswith cwd' file = drop (length cwd') file
					| otherwise = relPathDirToFile top' file
				file = Git.Filename.decode $ join sep $ take end bits
				value = bits !! end
				end = length bits - 1
				bits = split sep l
				sep = ": " ++ attr ++ ": "
				cwd' = cwd ++ "/"
				top' = top ++ "/"

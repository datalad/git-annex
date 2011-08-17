{- git-annex file copying
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.CopyFile (copyFile) where

import System.Directory (doesFileExist, removeFile)

import Utility
import qualified SysConfig

{- The cp command is used, because I hate reinventing the wheel,
 - and because this allows easy access to features like cp --reflink. -}
copyFile :: FilePath -> FilePath -> IO Bool
copyFile src dest = do
	whenM (doesFileExist dest) $
		removeFile dest
	boolSystem "cp" [params, File src, File dest]
	where
		params
			| SysConfig.cp_reflink_auto = Params "--reflink=auto"
			| SysConfig.cp_a = Params "-a"
			| SysConfig.cp_p = Params "-p"
			| otherwise = Params ""

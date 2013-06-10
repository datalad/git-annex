{- file copying
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.CopyFile (
	copyFileExternal,
	createLinkOrCopy
) where

import Common
import qualified Build.SysConfig as SysConfig

{- The cp command is used, because I hate reinventing the wheel,
 - and because this allows easy access to features like cp --reflink. -}
copyFileExternal :: FilePath -> FilePath -> IO Bool
copyFileExternal src dest = do
	whenM (doesFileExist dest) $
		removeFile dest
	boolSystem "cp" $ params ++ [File src, File dest]
  where
	params = map snd $ filter fst
		[ (SysConfig.cp_reflink_auto, Param "--reflink=auto")
		, (SysConfig.cp_a, Param "-a")
		, (SysConfig.cp_p && not SysConfig.cp_a, Param "-p")
		]

{- Create a hard link if the filesystem allows it, and fall back to copying
 - the file. -}
createLinkOrCopy :: FilePath -> FilePath -> IO Bool
#ifndef __WINDOWS__
createLinkOrCopy src dest = go `catchIO` const fallback
  where
  	go = do
		createLink src dest
		return True
  	fallback = copyFileExternal src dest
#else
createLinkOrCopy = copyFileExternal
#endif

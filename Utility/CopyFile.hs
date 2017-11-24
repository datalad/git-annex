{- file copying
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.CopyFile (
	copyFileExternal,
	createLinkOrCopy,
	CopyMetaData(..)
) where

import Common
import qualified Build.SysConfig as SysConfig

data CopyMetaData 
	-- Copy timestamps when possible, but no other metadata, and
	-- when copying a symlink, makes a copy of its content.
	= CopyTimeStamps
	-- Copy all metadata when possible.
	| CopyAllMetaData
	deriving (Eq)

{- The cp command is used, because I hate reinventing the wheel,
 - and because this allows easy access to features like cp --reflink. -}
copyFileExternal :: CopyMetaData -> FilePath -> FilePath -> IO Bool
copyFileExternal meta src dest = do
	whenM (doesFileExist dest) $
		removeFile dest
	boolSystem "cp" $ params ++ [File src, File dest]
  where
#ifndef __ANDROID__
	params = map snd $ filter fst
		[ (SysConfig.cp_reflink_auto, Param "--reflink=auto")
		, (allmeta && SysConfig.cp_a, Param "-a")
		, (allmeta && SysConfig.cp_p && not SysConfig.cp_a
			, Param "-p")
		, (not allmeta && SysConfig.cp_preserve_timestamps
			, Param "--preserve=timestamps")
		]
#else
	params = if allmeta then [] else []
#endif
	allmeta = meta == CopyAllMetaData

{- Create a hard link if the filesystem allows it, and fall back to copying
 - the file. -}
createLinkOrCopy :: FilePath -> FilePath -> IO Bool
createLinkOrCopy src dest = go `catchIO` const fallback
  where
	go = do
		createLink src dest
		return True
	fallback = copyFileExternal CopyAllMetaData src dest

{- Bundled programs
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Build.BundledPrograms where

import Data.Maybe

import Build.SysConfig as SysConfig

{- Programs that git-annex uses, to include in the bundle.
 -
 - These may be just the command name, or the full path to it. -}
bundledPrograms :: [FilePath]
bundledPrograms = catMaybes
	[ Nothing
#ifndef mingw32_HOST_OS
	-- git is not included in the windows bundle
	, Just "git"
#endif
	, Just "cp"
	, Just "xargs"
	, Just "rsync"
	, Just "ssh"
#ifndef mingw32_HOST_OS
	, Just "sh"
#endif
	, ifset SysConfig.gpg "gpg"
	, ifset SysConfig.curl "curl"
	, ifset SysConfig.wget "wget"
	, ifset SysConfig.bup "bup"
	, SysConfig.lsof
	, SysConfig.sha1
	, SysConfig.sha256
	, SysConfig.sha512
	, SysConfig.sha224
	, SysConfig.sha384
	]
  where
	ifset True s = Just s
	ifset False _ = Nothing

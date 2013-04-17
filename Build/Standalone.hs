{- Makes standalone bundle.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

import Control.Applicative
import Control.Monad.IfElse
import System.Environment
import Data.Maybe
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import Data.List
import Build.SysConfig as SysConfig

import Utility.PartialPrelude
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.SafeCommand
import Utility.Path

{- Programs that git-annex uses, to include in the bundle.
 -
 - These may be just the command name, or the full path to it. -}
thirdpartyProgs :: [FilePath]
thirdpartyProgs = catMaybes
	[ Just "git"
	, Just "cp"
	, Just "xargs"
	, Just "gpg"
	, Just "rsync"
	, Just "ssh"
	, Just "sh"
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

progDir :: FilePath -> FilePath
#ifdef darwin_HOST_OS
progDir topdir = topdir
#else
progDir topdir = topdir </> "bin"
#endif

installProg :: FilePath -> FilePath -> IO ()
installProg dir prog = searchPath prog >>= go
  where
	go Nothing = error $ "cannot find " ++ prog ++ " in PATH"
	go (Just f) = unlessM (boolSystem "install" [File f, File dir]) $
		error $ "install failed for " ++ prog

main = getArgs >>= go
  where
	go [] = error "specify topdir"
        go (topdir:_) = do
		let dir = progDir topdir
		createDirectoryIfMissing True dir
		forM_ thirdpartyProgs $ installProg dir
		

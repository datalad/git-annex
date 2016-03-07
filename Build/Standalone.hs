{- Makes standalone bundle.
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.IfElse
import System.Environment
import System.FilePath
import System.Directory
import Control.Monad
import Build.BundledPrograms

import Utility.SafeCommand
import Utility.Path

progDir :: FilePath -> FilePath
#ifdef darwin_HOST_OS
progDir topdir = topdir
#else
progDir topdir = topdir </> "bin"
#endif

extraProgDir :: FilePath -> FilePath
extraProgDir topdir = topdir </> "extra"

installProg :: FilePath -> FilePath -> IO (FilePath, FilePath)
installProg dir prog = searchPath prog >>= go
  where
	go Nothing = error $ "cannot find " ++ prog ++ " in PATH"
	go (Just f) = do
		let dest = dir </> takeFileName f
		unlessM (boolSystem "install" [File f, File dest]) $
			error $ "install failed for " ++ prog
		return (dest, f)

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify topdir"
	go (topdir:_) = do
		installed <- forM
			[ (progDir topdir, preferredBundledPrograms)
			, (extraProgDir topdir, extraBundledPrograms) ] $ \(dir, progs) -> do
			createDirectoryIfMissing True dir
			forM progs $ installProg dir
		writeFile "tmp/standalone-installed" (show (concat installed))

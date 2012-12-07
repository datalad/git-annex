{- OSX library copier
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Build.OSXMkLibs where

import Control.Applicative
import System.Environment
import Data.Maybe
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import Data.List

import Utility.PartialPrelude
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.SafeCommand
import Utility.Path

{- Recursively find and install libs, until nothing new to install is found. -}
mklibs :: FilePath -> [FilePath] -> IO [FilePath]
mklibs appbase libdirs = do
	new <- catMaybes <$> installLibs appbase
	if null new
		then return (libdirs++new)
		else mklibs appbase (libdirs++new)

{- Returns directories into which new libs were installed. -}
installLibs :: FilePath -> IO [Maybe FilePath]
installLibs appbase = do
	needlibs <- otool appbase
	forM needlibs $ \lib -> do
		let libdir = parentDir lib
		let dest = appbase ++ lib
		ifM (doesFileExist dest)
			( return Nothing
			, do
				createDirectoryIfMissing True (appbase ++ libdir)
				_ <- boolSystem "cp" [File lib, File dest]
				putStrLn $ "installing " ++ lib
				return $ Just libdir
			)

otool :: FilePath -> IO [FilePath]
otool appbase = do
	files <- filterM doesFileExist =<< dirContentsRecursive appbase
	s <- readProcess "otool" ("-L" : files)
	return $ nub $ parseOtool s

parseOtool :: String -> [FilePath]
parseOtool = catMaybes . map parse . lines
  where
	parse l
		| "\t" `isPrefixOf` l = headMaybe $ words l
		| otherwise = Nothing

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify OSXAPP_BASE"
	go (appbase:_) = do
		libdirs <- mklibs appbase []
		writeFile (appbase </> "libdirs") $
			unlines $ nub libdirs

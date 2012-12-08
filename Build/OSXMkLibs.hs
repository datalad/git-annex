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
		let dest = appbase </> takeFileName lib
		ifM (doesFileExist dest)
			( return Nothing
			, do
				createDirectoryIfMissing True appbase
				putStrLn $ "installing " ++ lib
				_ <- boolSystem "cp" [File lib, File dest]
				return $ Just appbase
			)

{- Returns libraries to install. -}
otool :: FilePath -> IO [FilePath]
otool appbase = do
	files <- filterM doesFileExist =<< dirContentsRecursive appbase
	l <- forM files $ \file -> do
		libs <- filter unprocessed . parseOtool
			<$> readProcess "otool" ["-L", file]
		forM_ libs $ \lib -> install_name_tool file lib
		return libs
	return $ nub $ concat l
  where
	unprocessed s = not ("@executable_path" `isInfixOf` s)

parseOtool :: String -> [FilePath]
parseOtool = catMaybes . map parse . lines
  where
	parse l
		| "\t" `isPrefixOf` l = headMaybe $ words l
		| otherwise = Nothing

{- Adjusts binaries to use libraries in paths relative to the executable.
 -
 - Assumes all executables are installed into the same directory, and
 - the libraries will be installed in subdirectories that match their
 - absolute paths. -}
install_name_tool :: FilePath -> FilePath -> IO ()
install_name_tool binary lib = do
	ok <- boolSystem "install_name_tool"
		[ Param "-change"
		, File lib
		, Param $ "@executable_path" ++ (dropFileName lib)
		, File binary
		]
	unless ok $
		hPutStrLn stderr $ "install_name_tool failed for " ++ binary

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify OSXAPP_BASE"
	go (appbase:_) = do
		libdirs <- mklibs appbase []
		writeFile (appbase </> "libdirs") $
			unlines $ nub libdirs

{- OSX library copier
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

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

import qualified Data.Map as M
import qualified Data.Set as S

type LibMap = M.Map FilePath String

{- Recursively find and install libs, until nothing new to install is found. -}
mklibs :: FilePath -> [FilePath] -> LibMap -> IO ()
mklibs appbase libdirs libmap = do
	(new, libmap') <- installLibs appbase libmap
	unless (null new) $
		mklibs appbase (libdirs++new) libmap'

{- Returns directories into which new libs were installed. -}
installLibs :: FilePath -> LibMap -> IO ([FilePath], LibMap)
installLibs appbase libmap = do
	(needlibs, libmap') <- otool appbase libmap
	libs <- forM needlibs $ \lib -> do
		let shortlib = fromMaybe (error "internal") (M.lookup lib libmap')
		let fulllib = dropWhile (== '/') lib
		let dest = appbase </> fulllib
		let symdest = appbase </> shortlib
		ifM (doesFileExist dest)
			( return Nothing
			, do
				createDirectoryIfMissing True (parentDir dest)
				putStrLn $ "installing " ++ lib ++ " as " ++ shortlib
				_ <- boolSystem "cp" [File lib, File dest]
				_ <- boolSystem "chmod" [Param "644", File dest]
				_ <- boolSystem "ln" [Param "-s", File fulllib, File symdest]
				return $ Just appbase
			)
	return (catMaybes libs, libmap')

{- Returns libraries to install. -}
otool :: FilePath -> LibMap -> IO ([FilePath], LibMap)
otool appbase libmap = do
	files <- filterM doesFileExist =<< dirContentsRecursive appbase
	process [] files libmap
  where
	want s = not ("@executable_path" `isInfixOf` s)
		&& not (".framework" `isInfixOf` s)
		&& not ("libSystem.B" `isInfixOf` s)
	process c [] m = return (nub $ concat c, m)
	process c (file:rest) m = do
		_ <- boolSystem "chmod" [Param "755", File file]
		libs <- filter want . parseOtool
			<$> readProcess "otool" ["-L", file]
		m' <- install_name_tool file libs m
		process (libs:c) rest m'

parseOtool :: String -> [FilePath]
parseOtool = catMaybes . map parse . lines
  where
	parse l
		| "\t" `isPrefixOf` l = headMaybe $ words l
		| otherwise = Nothing

{- Adjusts binaries to use libraries bundled with it, rather than the
 - system libraries. -}
install_name_tool :: FilePath -> [FilePath] -> LibMap -> IO LibMap
install_name_tool _ [] libmap = return libmap
install_name_tool binary libs libmap = do
	let (libnames, libmap') = getLibNames libs libmap
	let params = concatMap change $ zip libs libnames
	ok <- boolSystem "install_name_tool" $ params ++ [File binary]
	unless ok $
		error $ "install_name_tool failed for " ++ binary
	return libmap'
  where
	change (lib, libname) =
		[ Param "-change"
		, File lib
		, Param $ "@executable_path/" ++ libname
		]

getLibNames :: [FilePath] -> LibMap -> ([FilePath], LibMap)
getLibNames libs libmap = go [] libs libmap
  where
	go c [] m = (reverse c, m)
	go c (l:rest) m =
		let (f, m') = getLibName l m
		in go (f:c) rest m'

{- Uses really short names for the library files it installs, because
 - binaries have arbitrarily short RPATH field limits. -}
getLibName :: FilePath -> LibMap -> (FilePath, LibMap)
getLibName lib libmap = case M.lookup lib libmap of
	Just n -> (n, libmap)
	Nothing -> (nextfreename, M.insert lib nextfreename libmap)
  where
	names = map (\c -> [c]) ['A' .. 'Z'] ++
		[[n, l] | n <- ['0' .. '9'], l <- ['A' .. 'Z']]
	used = S.fromList $ M.elems libmap
	nextfreename = fromMaybe (error "ran out of short library names!") $ 
		headMaybe $ dropWhile (`S.member` used) names

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify OSXAPP_BASE"
	go (appbase:_) = mklibs appbase [] M.empty

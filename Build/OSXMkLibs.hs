{- OSX library copier
 -
 - Copyright 2012-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Build.OSXMkLibs (mklibs) where

import Data.Maybe
import System.IO
import Control.Monad
import Control.Monad.IfElse
import Data.List
import Control.Applicative
import System.Posix.Files
import Prelude

import Utility.PartialPrelude
import Utility.OsPath
import Utility.Directory
import Utility.SystemDirectory
import Utility.Process
import Utility.Monad
import Utility.SafeCommand
import Utility.Path
import Utility.Exception
import Utility.Env
import Utility.Split
import Utility.FileSystemEncoding
import qualified Utility.OsString as OS

import qualified Data.Map as M
import qualified Data.Set as S

type LibMap = M.Map OsPath String

mklibs :: OsPath -> M.Map OsPath OsPath -> IO Bool
mklibs appbase installedbins = do
	mklibs' appbase installedbins [] [] M.empty
	return True

{- Recursively find and install libs, until nothing new to install is found. -}
mklibs' :: OsPath -> M.Map OsPath OsPath -> [OsPath] -> [(OsPath, OsPath)] -> LibMap -> IO ()
mklibs' appbase installedbins libdirs replacement_libs libmap = do
	(new, replacement_libs', libmap') <- installLibs appbase installedbins replacement_libs libmap
	unless (null new) $
		mklibs' appbase installedbins (libdirs++new) replacement_libs' libmap'

{- Returns directories into which new libs were installed. -}
installLibs :: OsPath -> M.Map OsPath OsPath -> [(OsPath, OsPath)] -> LibMap -> IO ([OsPath], [(OsPath, OsPath)], LibMap)
installLibs appbase installedbins replacement_libs libmap = do
	(needlibs, replacement_libs', libmap') <- otool appbase installedbins replacement_libs libmap
	libs <- forM needlibs $ \lib -> do
		pathlib <- findLibPath lib
		let shortlib = toOsPath $ fromMaybe (error "internal") (M.lookup lib libmap')
		let fulllib = OS.dropWhile (== unsafeFromChar '/') lib
		let dest = appbase </> fulllib
		let symdest = appbase </> shortlib
		-- This is a hack; libraries need to be in the same
		-- directory as the program, so also link them into the
		-- extra and git-core directories so programs in those will
		-- find them.
		let symdestextra = 
			[ appbase </> literalOsPath "extra" </> shortlib
			, appbase </> literalOsPath "git-core" </> shortlib
			]
		ifM (doesFileExist dest)
			( return Nothing
			, do
				createDirectoryIfMissing True (parentDir dest)
				putStrLn $ "installing " ++ fromOsPath pathlib ++ " as " ++ fromOsPath shortlib
				unlessM (boolSystem "cp" [File (fromOsPath pathlib), File (fromOsPath dest)]
					<&&> boolSystem "chmod" [Param "644", File (fromOsPath dest)]
					<&&> boolSystem "ln" [Param "-s", File (fromOsPath fulllib), File (fromOsPath symdest)]) $
					error "library install failed"
				forM_ symdestextra $ \d ->
					unlessM (boolSystem "ln" [Param "-s", File (fromOsPath (literalOsPath ".." </> fulllib)), File (fromOsPath d)]) $
						error "library linking failed"
				return $ Just appbase
			)
	return (catMaybes libs, replacement_libs', libmap')

{- Returns libraries to install.
 -
 - Note that otool -L ignores DYLD_LIBRARY_PATH, so the
 - library files returned may need to be run through findLibPath
 - to find the actual libraries to install.
 -}
otool :: OsPath -> M.Map OsPath OsPath -> [(OsPath, OsPath)] -> LibMap -> IO ([OsPath], [(OsPath, OsPath)], LibMap)
otool appbase installedbins replacement_libs libmap = do
	files <- filterM doesFileExist =<< dirContentsRecursive appbase
	process [] files replacement_libs libmap
  where
	want s = 
		-- This seems to be about finding libraries next
		-- to executables or something. May need further resolving
		-- to find real path to library, but for now, skip it.
		not ("@executable_path" `isInfixOf` s)
		&& not ("@loader_path" `isInfixOf` s)
		-- OSX frameworks such as Cocoa are too tightly tied to
		-- a specific OSX version, so don't bundle.
		&& not (".framework" `isInfixOf` s)
		-- libSystem.B is tightly tied to frameworks.
		&& not ("libSystem.B" `isInfixOf` s)
		-- ImageIO.framework uses libPng which is built against a
		-- specific version of libz; other versions lack the
		-- _inflateValidate symbol. So, avoid bundling libz unless
		-- this incompatibility is resolved.
		&& not ("libz." `isInfixOf` s)
	lib_present s
		| "@rpath" `isInfixOf` s = return True
		| otherwise = ifM (isJust <$> catchMaybeIO (getSymbolicLinkStatus s))
			( return True
			, do
				hPutStrLn stderr $ "note: skipping library that is not present on disk: " ++ s
				return False
			)
	process c [] rls m = return (nub $ concat c, rls, m)
	process c (file:rest) rls m = do
		_ <- boolSystem "chmod" [Param "755", File (fromOsPath file)]
		libs <- filterM lib_present
			=<< filter want . parseOtool
			<$> readProcess "otool" ["-L", fromOsPath file]
		expanded_libs <- expand_rpath installedbins libs replacement_libs file
		let rls' = nub $ rls ++ (zip (map toOsPath libs) expanded_libs)
		m' <- install_name_tool file (map toOsPath libs) expanded_libs m
		process (expanded_libs:c) rest rls' m'

findLibPath :: OsPath -> IO OsPath
findLibPath l = go =<< getEnv "DYLD_LIBRARY_PATH"
  where
	go Nothing = return l
	go (Just p) = fromMaybe l
		<$> firstM doesFileExist (map (\p' -> toOsPath p' </> f) (splitc ':' p))
	f = takeFileName l

{- Expands any @rpath in the list of libraries.
 -
 - This is done by the nasty method of running the command with a dummy
 - option (so it doesn't do anything.. hopefully!) and asking the dynamic
 - linker to print expanded rpaths.
 -}
expand_rpath :: M.Map OsPath OsPath -> [String] -> [(OsPath, OsPath)] -> OsPath -> IO [OsPath]
expand_rpath installedbins libs replacement_libs cmd
	| any ("@rpath" `isInfixOf`) libs = do
		let origcmd = case M.lookup cmd installedbins of
			Nothing -> cmd
			Just cmd' -> cmd'
		s <- catchDefaultIO "" $ readProcess "sh" ["-c", probe origcmd]
		let m = if (null s)
			then M.fromList replacement_libs
			else M.fromList $ mapMaybe parse $ lines s
		return $ map (replacem m . toOsPath) libs
	| otherwise = return (map toOsPath libs)
  where
	probe c = "DYLD_PRINT_RPATHS=1 " ++ fromOsPath c ++ " --getting-rpath-dummy-option 2>&1 | grep RPATH"
	parse s = case words s of
		("RPATH":"successful":"expansion":"of":old:"to:":new:[]) -> 
			Just (toOsPath old, toOsPath new)
		_ -> Nothing
	replacem m l = fromMaybe l $ M.lookup (toOsPath l) m

parseOtool :: String -> [String]
parseOtool = catMaybes . map parse . lines
  where
	parse l
		| "\t" `isPrefixOf` l = headMaybe $ words l
		| otherwise = Nothing

{- Adjusts binaries to use libraries bundled with it, rather than the
 - system libraries. -}
install_name_tool :: OsPath -> [OsPath] -> [OsPath] -> LibMap -> IO LibMap
install_name_tool _ [] _ libmap = return libmap
install_name_tool binary libs expanded_libs libmap = do
	let (libnames, libmap') = getLibNames expanded_libs libmap
	let params = concatMap change $ zip libs libnames
	ok <- boolSystem "install_name_tool" $ params ++ [File (fromOsPath binary)]
	unless ok $
		error $ "install_name_tool failed for " ++ fromOsPath binary
	return libmap'
  where
	change (lib, libname) =
		[ Param "-change"
		, File (fromOsPath lib)
		, Param $ "@executable_path/" ++ fromOsPath libname
		]

getLibNames :: [OsPath] -> LibMap -> ([OsPath], LibMap)
getLibNames libs libmap = go [] libs libmap
  where
	go c [] m = (reverse c, m)
	go c (l:rest) m =
		let (f, m') = getLibName l m
		in go (f:c) rest m'

{- Uses really short names for the library files it installs, because
 - binaries have arbitrarily short RPATH field limits. -}
getLibName :: OsPath -> LibMap -> (OsPath, LibMap)
getLibName lib libmap = case M.lookup lib libmap of
	Just n -> (toOsPath n, libmap)
	Nothing -> (toOsPath nextfreename, M.insert lib nextfreename libmap)
  where
	names = map pure ['A' .. 'Z'] ++
		[[n, l] | n <- ['0' .. '9'], l <- ['A' .. 'Z']]
	used = S.fromList $ M.elems libmap
	nextfreename = fromMaybe (error "ran out of short library names!") $ 
		headMaybe $ dropWhile (`S.member` used) names

{- path manipulation
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Path (
	simplifyPath,
	absPathFrom,
	parentDir,
	upFrom,
	dirContains,
	absPath,
	relPathCwdToFile,
	relPathDirToFile,
	relPathDirToFileAbs,
	segmentPaths,
	segmentPaths',
	runSegmentPaths,
	runSegmentPaths',
	relHome,
	inPath,
	searchPath,
	dotfile,
	splitShortExtensions,

	prop_upFrom_basics,
	prop_relPathDirToFile_basics,
	prop_relPathDirToFile_regressionTest,
) where

import System.FilePath.ByteString
import qualified System.FilePath as P
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
#ifdef mingw32_HOST_OS
import Data.Char
#else
import System.Posix.Directory.ByteString (getWorkingDirectory)
#endif
import Control.Applicative
import Prelude

import Utility.Monad
import Utility.UserInfo
import Utility.SystemDirectory
import Utility.Split
import Utility.FileSystemEncoding

{- Simplifies a path, removing any "." component, collapsing "dir/..", 
 - and removing the trailing path separator.
 -
 - On Windows, preserves whichever style of path separator might be used in
 - the input RawFilePaths. This is done because some programs in Windows
 - demand a particular path separator -- and which one actually varies!
 -
 - This does not guarantee that two paths that refer to the same location,
 - and are both relative to the same location (or both absolute) will
 - yeild the same result. Run both through normalise from System.RawFilePath
 - to ensure that.
 -}
simplifyPath :: RawFilePath -> RawFilePath
simplifyPath path = dropTrailingPathSeparator $ 
	joinDrive drive $ joinPath $ norm [] $ splitPath path'
  where
	(drive, path') = splitDrive path

	norm c [] = reverse c
	norm c (p:ps)
		| p' == ".." && not (null c) && dropTrailingPathSeparator (c !! 0) /= ".." = 
			norm (drop 1 c) ps
		| p' == "." = norm c ps
		| otherwise = norm (p:c) ps
	  where
		p' = dropTrailingPathSeparator p

{- Makes a path absolute.
 -
 - Also simplifies it using simplifyPath.
 -
 - The first parameter is a base directory (ie, the cwd) to use if the path
 - is not already absolute, and should itsef be absolute.
 -
 - Does not attempt to deal with edge cases or ensure security with
 - untrusted inputs.
 -}
absPathFrom :: RawFilePath -> RawFilePath -> RawFilePath
absPathFrom dir path = simplifyPath (combine dir path)

{- takeDirectory "foo/bar/" is "foo/bar". This instead yields "foo" -}
parentDir :: RawFilePath -> RawFilePath
parentDir = takeDirectory . dropTrailingPathSeparator

{- Just the parent directory of a path, or Nothing if the path has no
- parent (ie for "/" or "." or "foo") -}
upFrom :: RawFilePath -> Maybe RawFilePath
upFrom dir
	| length dirs < 2 = Nothing
	| otherwise = Just $ joinDrive drive $
		B.intercalate (B.singleton pathSeparator) $ init dirs
  where
	-- on Unix, the drive will be "/" when the dir is absolute,
	-- otherwise ""
	(drive, path) = splitDrive dir
	dirs = filter (not . B.null) $ B.splitWith isPathSeparator path

prop_upFrom_basics :: RawFilePath -> Bool
prop_upFrom_basics dir
	| B.null dir = True
	| dir == "/" = p == Nothing
	| otherwise = p /= Just dir
  where
	p = upFrom dir

{- Checks if the first RawFilePath is, or could be said to contain the second.
 - For example, "foo/" contains "foo/bar". Also, "foo", "./foo", "foo/" etc
 - are all equivilant.
 -}
dirContains :: RawFilePath -> RawFilePath -> Bool
dirContains a b = a == b
	|| a' == b'
	|| (addTrailingPathSeparator a') `B.isPrefixOf` b'
	|| a' == "." && normalise ("." </> b') == b'
  where
	a' = norm a
	b' = norm b
	norm = normalise . simplifyPath

{- Converts a filename into an absolute path.
 -
 - Also simplifies it using simplifyPath.
 -
 - Unlike Directory.canonicalizePath, this does not require the path
 - already exists. -}
absPath :: RawFilePath -> IO RawFilePath
absPath file
	-- Avoid unncessarily getting the current directory when the path
	-- is already absolute. absPathFrom uses simplifyPath
	-- so also used here for consistency.
	| isAbsolute file = return $ simplifyPath file
	| otherwise = do
#ifdef mingw32_HOST_OS
		cwd <- toRawFilePath <$> getCurrentDirectory
#else
		cwd <- getWorkingDirectory
#endif
		return $ absPathFrom cwd file

{- Constructs a relative path from the CWD to a file.
 -
 - For example, assuming CWD is /tmp/foo/bar:
 -    relPathCwdToFile "/tmp/foo" == ".."
 -    relPathCwdToFile "/tmp/foo/bar" == "" 
 -}
relPathCwdToFile :: RawFilePath -> IO RawFilePath
relPathCwdToFile f = do
#ifdef mingw32_HOST_OS
	c <- toRawFilePath <$> getCurrentDirectory
#else
	c <- getWorkingDirectory
#endif
	relPathDirToFile c f

{- Constructs a relative path from a directory to a file. -}
relPathDirToFile :: RawFilePath -> RawFilePath -> IO RawFilePath
relPathDirToFile from to = relPathDirToFileAbs <$> absPath from <*> absPath to

{- This requires the first path to be absolute, and the
 - second path cannot contain ../ or ./
 -
 - On Windows, if the paths are on different drives,
 - a relative path is not possible and the path is simply
 - returned as-is.
 -}
relPathDirToFileAbs :: RawFilePath -> RawFilePath -> RawFilePath
relPathDirToFileAbs from to
#ifdef mingw32_HOST_OS
	| normdrive from /= normdrive to = to
#endif
	| otherwise = joinPath $ dotdots ++ uncommon
  where
	pfrom = sp from
	pto = sp to
	sp = map dropTrailingPathSeparator . splitPath . dropDrive
	common = map fst $ takeWhile same $ zip pfrom pto
	same (c,d) = c == d
	uncommon = drop numcommon pto
	dotdots = replicate (length pfrom - numcommon) ".."
	numcommon = length common
#ifdef mingw32_HOST_OS
	normdrive = map toLower . takeWhile (/= ':') . fromRawFilePath . takeDrive
#endif

prop_relPathDirToFile_basics :: RawFilePath -> RawFilePath -> Bool
prop_relPathDirToFile_basics from to
	| B.null from || B.null to = True
	| from == to = B.null r
	| otherwise = not (B.null r)
  where
	r = relPathDirToFileAbs from to 

prop_relPathDirToFile_regressionTest :: Bool
prop_relPathDirToFile_regressionTest = same_dir_shortcurcuits_at_difference
  where
	{- Two paths have the same directory component at the same
	 - location, but it's not really the same directory.
	 - Code used to get this wrong. -}
	same_dir_shortcurcuits_at_difference =
		relPathDirToFileAbs (joinPath [pathSeparator `B.cons` "tmp", "r", "lll", "xxx", "yyy", "18"])
			(joinPath [pathSeparator `B.cons` "tmp", "r", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"])
				== joinPath ["..", "..", "..", "..", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"]

{- Given an original list of paths, and an expanded list derived from it,
 - which may be arbitrarily reordered, generates a list of lists, where
 - each sublist corresponds to one of the original paths.
 -
 - When the original path is a directory, any items in the expanded list
 - that are contained in that directory will appear in its segment.
 -
 - The order of the original list of paths is attempted to be preserved in
 - the order of the returned segments. However, doing so has a O^NM
 - growth factor. So, if the original list has more than 100 paths on it,
 - we stop preserving ordering at that point. Presumably a user passing
 - that many paths in doesn't care too much about order of the later ones.
 -}
segmentPaths :: (a -> RawFilePath) -> [RawFilePath] -> [a] -> [[a]]
segmentPaths = segmentPaths' (\_ r -> r)

segmentPaths' :: (Maybe RawFilePath -> a -> r) -> (a -> RawFilePath) -> [RawFilePath] -> [a] -> [[r]]
segmentPaths' f _ [] new = [map (f Nothing) new]
segmentPaths' f _ [i] new = [map (f (Just i)) new] -- optimisation
segmentPaths' f c (i:is) new = 
	map (f (Just i)) found : segmentPaths' f c is rest
  where
	(found, rest) = if length is < 100
		then partition ini new
		else break (not . ini) new
	ini p = i `dirContains` c p

{- This assumes that it's cheaper to call segmentPaths on the result,
 - than it would be to run the action separately with each path. In
 - the case of git file list commands, that assumption tends to hold.
 -}
runSegmentPaths :: (a -> RawFilePath) -> ([RawFilePath] -> IO [a]) -> [RawFilePath] -> IO [[a]]
runSegmentPaths c a paths = segmentPaths c paths <$> a paths

runSegmentPaths' :: (Maybe RawFilePath -> a -> r) -> (a -> RawFilePath) -> ([RawFilePath] -> IO [a]) -> [RawFilePath] -> IO [[r]]
runSegmentPaths' si c a paths = segmentPaths' si c paths <$> a paths

{- Converts paths in the home directory to use ~/ -}
relHome :: FilePath -> IO String
relHome path = do
	let path' = toRawFilePath path
	home <- toRawFilePath <$> myHomeDir
	return $ if dirContains home path'
		then fromRawFilePath ("~/" <> relPathDirToFileAbs home path')
		else path

{- Checks if a command is available in PATH.
 -
 - The command may be fully-qualified, in which case, this succeeds as
 - long as it exists. -}
inPath :: String -> IO Bool
inPath command = isJust <$> searchPath command

{- Finds a command in PATH and returns the full path to it.
 -
 - The command may be fully qualified already, in which case it will
 - be returned if it exists.
 -
 - Note that this will find commands in PATH that are not executable.
 -}
searchPath :: String -> IO (Maybe FilePath)
searchPath command
	| P.isAbsolute command = check command
	| otherwise = P.getSearchPath >>= getM indir
  where
	indir d = check $ d P.</> command
	check f = firstM doesFileExist
#ifdef mingw32_HOST_OS
		[f, f ++ ".exe"]
#else
		[f]
#endif

{- Checks if a filename is a unix dotfile. All files inside dotdirs
 - count as dotfiles. -}
dotfile :: RawFilePath -> Bool
dotfile file
	| f == "." = False
	| f == ".." = False
	| f == "" = False
	| otherwise = "." `B.isPrefixOf` f || dotfile (takeDirectory file)
  where
	f = takeFileName file

{- Similar to splitExtensions, but knows that some things in RawFilePaths
 - after a dot are too long to be extensions. -}
splitShortExtensions :: RawFilePath -> (RawFilePath, [B.ByteString])
splitShortExtensions = splitShortExtensions' 5 -- enough for ".jpeg"
splitShortExtensions' :: Int -> RawFilePath -> (RawFilePath, [B.ByteString])
splitShortExtensions' maxextension = go []
  where
	go c f
		| len > 0 && len <= maxextension && not (B.null base) = 
			go (ext:c) base
		| otherwise = (f, c)
	  where
		(base, ext) = splitExtension f
		len = B.length ext

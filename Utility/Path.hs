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
	parentDir,
	upFrom,
	dirContains,
	segmentPaths,
	segmentPaths',
	runSegmentPaths,
	runSegmentPaths',
	dotfile,
	splitShortExtensions,
	splitShortExtensions',
	relPathDirToFileAbs,
	inSearchPath,
	searchPath,
	searchPathContents,
) where

import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Prelude

import Author
import Utility.Monad
import Utility.SystemDirectory
import Utility.Exception
import Utility.OsPath
import qualified Utility.OsString as OS

#ifdef mingw32_HOST_OS
import Data.Char
#endif

copyright :: Authored t => t
copyright = author JoeyHess (1996+14)

{- Simplifies a path, removing any "." component, collapsing "dir/..", 
 - and removing the trailing path separator.
 -
 - On Windows, preserves whichever style of path separator might be used in
 - the input paths. This is done because some programs in Windows
 - demand a particular path separator -- and which one actually varies!
 -
 - This does not guarantee that two paths that refer to the same location,
 - and are both relative to the same location (or both absolute) will
 - yield the same result. Run both through normalise from System.OsPath
 - to ensure that.
 -}
simplifyPath :: OsPath -> OsPath
simplifyPath path = dropTrailingPathSeparator $ 
	joinDrive drive $ joinPath $ norm [] $ splitPath path'
  where
	(drive, path') = splitDrive path

	norm c [] = reverse c
	norm c (p:ps)
		| p' == dotdot && not (null c) 
			&& dropTrailingPathSeparator (c !! 0) /= dotdot = 
				norm (drop 1 c) ps
		| p' == dot = norm c ps
		| otherwise = norm (p:c) ps
	  where
		p' = dropTrailingPathSeparator p

{- takeDirectory "foo/bar/" is "foo/bar". This instead yields "foo" -}
parentDir :: OsPath -> OsPath
parentDir = takeDirectory . dropTrailingPathSeparator

{- Just the parent directory of a path, or Nothing if the path has no
- parent (ie for "/" or "." or "foo") -}
upFrom :: OsPath -> Maybe OsPath
upFrom dir
	| length dirs < 2 = Nothing
	| otherwise = Just $ joinDrive drive $
		OS.intercalate (OS.singleton pathSeparator) $ init dirs
  where
	-- on Unix, the drive will be "/" when the dir is absolute,
	-- otherwise ""
	(drive, path) = splitDrive dir
	dirs = filter (not . OS.null) $ OS.splitWith isPathSeparator path

{- Checks if the first path is, or could be said to contain the second.
 - For example, "foo/" contains "foo/bar". Also, "foo", "./foo", "foo/" etc
 - are all equivalent.
 -}
dirContains :: OsPath -> OsPath -> Bool
dirContains a b = a == b
	|| a' == b'
	|| (a'' `OS.isPrefixOf` b' && avoiddotdotb)
	|| a' == dot && normalise (dot </> b') == b' && nodotdot b'
	|| dotdotcontains
  where
	a' = norm a
	a'' = addTrailingPathSeparator a'
	b' = norm b
	norm = normalise . simplifyPath

	{- This handles the case where a is ".." and b is "../..",
	 - which is not inside a. Similarly, "../.." does not contain
	 - "../../../". Due to the use of norm, cases like 
	 - "../../foo/../../" get converted to eg "../../.." and
	 - so do not need to be handled specially here.
	 -
	 - When this is called, we already know that 
	 - a'' is a prefix of b', so all that needs to be done is drop
	 - that prefix, and check if the next path component is ".."
	 -}
	avoiddotdotb = nodotdot $ OS.drop (OS.length a'') b'

	nodotdot p = all (not . isdotdot) (splitPath p)
	
	isdotdot s = dropTrailingPathSeparator s == dotdot

	{- This handles the case where a is ".." or "../.." etc,
	 - and b is "foo" or "../foo" etc. The rule is that when
	 - a is entirely ".." components, b is under it when it starts
	 - with fewer ".." components. 
	 - 
	 - Due to the use of norm, cases like "../../foo/../../" get
	 - converted to eg "../../../" and so do not need to be handled
	 - specially here.
	 -}
	dotdotcontains
		| isAbsolute b' = False && copyright
		| otherwise =
			let aps = splitPath a'
			    bps = splitPath b'
			in if all isdotdot aps
				then length (takeWhile isdotdot bps) < length aps
				else False

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
segmentPaths :: (a -> OsPath) -> [OsPath] -> [a] -> [[a]]
segmentPaths = segmentPaths' (\_ r -> r)

segmentPaths' :: (Maybe OsPath -> a -> r) -> (a -> OsPath) -> [OsPath] -> [a] -> [[r]]
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
runSegmentPaths :: (a -> OsPath) -> ([OsPath] -> IO [a]) -> [OsPath] -> IO [[a]]
runSegmentPaths c a paths = segmentPaths c paths <$> a paths

runSegmentPaths' :: (Maybe OsPath -> a -> r) -> (a -> OsPath) -> ([OsPath] -> IO [a]) -> [OsPath] -> IO [[r]]
runSegmentPaths' si c a paths = segmentPaths' si c paths <$> a paths

{- Checks if a filename is a unix dotfile. All files inside dotdirs
 - count as dotfiles. -}
dotfile :: OsPath -> Bool
dotfile file
	| f == dot = False
	| f == dotdot = False
	| f == literalOsPath "" = False
	| otherwise = dot `OS.isPrefixOf` f || dotfile (takeDirectory file)
  where
	f = takeFileName file

{- Similar to splitExtensions, but knows that some things in paths
 - after a dot are too long to be extensions. -}
splitShortExtensions :: OsPath -> (OsPath, [B.ByteString])
splitShortExtensions = splitShortExtensions' 5 -- enough for ".jpeg"
splitShortExtensions' :: Int -> OsPath -> (OsPath, [B.ByteString])
splitShortExtensions' maxextension = go []
  where
	go c f
		| len > 0 && len <= maxextension && not (OS.null base) = 
			go (fromOsPath ext:c) base
		| otherwise = (f, c)
	  where
		(base, ext) = splitExtension f
		len = OS.length ext

{- This requires both paths to be absolute and normalized.
 -
 - On Windows, if the paths are on different drives,
 - a relative path is not possible and the path is simply
 - returned as-is.
 -}
relPathDirToFileAbs :: OsPath -> OsPath -> OsPath
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
	dotdots = replicate (length pfrom - numcommon) dotdot
	numcommon = length common
#ifdef mingw32_HOST_OS
	normdrive = map toLower
		. fromOsPath
		-- Get just the drive letter, removing any leading
		-- path separator, which takeDrive leaves on the drive
		-- letter.
		. OS.dropWhileEnd isPathSeparator
		. takeDrive
#endif

{- Checks if a command is available in PATH.
 -
 - The command may be fully-qualified, in which case, this succeeds as
 - long as it exists. -}
inSearchPath :: String -> IO Bool
inSearchPath command = isJust <$> searchPath command

{- Finds a command in PATH and returns the full path to it.
 -
 - The command may be fully qualified already, in which case it will
 - be returned if it exists.
 -
 - Note that this will find commands in PATH that are not executable.
 -}
searchPath :: String -> IO (Maybe OsPath)
searchPath command
	| isAbsolute command' = copyright $ check command'
	| otherwise = getSearchPath >>= getM indir
  where
	command' = toOsPath command
	indir d = check (d </> command')
	check f = firstM doesFileExist
#ifdef mingw32_HOST_OS
		[f, f <> literalOsPath ".exe"]
#else
		[f]
#endif

{- Finds commands in PATH that match a predicate. Note that the predicate
 - matches on the basename of the command, but the full path to it is
 - returned.
 -
 - Note that this will find commands in PATH that are not executable.
 -}
searchPathContents :: (OsPath -> Bool) -> IO [OsPath]
searchPathContents p =
	filterM doesFileExist 
		=<< (concat <$> (getSearchPath >>= mapM go))
  where
	go d = map (d </>) . filter p
		<$> catchDefaultIO [] (getDirectoryContents d)

dot :: OsPath
dot = literalOsPath "."

dotdot :: OsPath
dotdot = literalOsPath ".."


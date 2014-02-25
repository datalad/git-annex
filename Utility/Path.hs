{- path manipulation
 -
 - Copyright 2010-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE PackageImports, CPP #-}

module Utility.Path where

import Data.String.Utils
import System.FilePath
import System.Directory
import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative

#ifdef mingw32_HOST_OS
import qualified System.FilePath.Posix as Posix
#else
import System.Posix.Files
#endif

import qualified "MissingH" System.Path as MissingH
import Utility.Monad
import Utility.UserInfo

{- Simplifies a path, removing any ".." or ".", and removing the trailing
 - path separator.
 -
 - On Windows, preserves whichever style of path separator might be used in
 - the input FilePaths. This is done because some programs in Windows
 - demand a particular path separator -- and which one actually varies!
 -
 - This does not guarantee that two paths that refer to the same location,
 - and are both relative to the same location (or both absolute) will
 - yeild the same result. Run both through normalise from System.FilePath
 - to ensure that.
 -}
simplifyPath :: FilePath -> FilePath
simplifyPath path = dropTrailingPathSeparator $ 
	joinDrive drive $ joinPath $ norm [] $ splitPath path'
  where
	(drive, path') = splitDrive path

	norm c [] = reverse c
	norm c (p:ps)
		| p' == ".." = norm (drop 1 c) ps
		| p' == "." = norm c ps
		| otherwise = norm (p:c) ps
	  where
		p' = dropTrailingPathSeparator p

{- Makes a path absolute.
 -
 - The first parameter is a base directory (ie, the cwd) to use if the path
 - is not already absolute.
 -
 - Does not attempt to deal with edge cases or ensure security with
 - untrusted inputs.
 -}
absPathFrom :: FilePath -> FilePath -> FilePath
absPathFrom dir path = simplifyPath (combine dir path)

{- On Windows, this converts the paths to unix-style, in order to run
 - MissingH's absNormPath on them. Resulting path will use / separators. -}
absNormPathUnix :: FilePath -> FilePath -> Maybe FilePath
#ifndef mingw32_HOST_OS
absNormPathUnix dir path = MissingH.absNormPath dir path
#else
absNormPathUnix dir path = todos <$> MissingH.absNormPath (fromdos dir) (fromdos path)
  where
	fromdos = replace "\\" "/"
	todos = replace "/" "\\"
#endif

{- Returns the parent directory of a path.
 -
 - To allow this to be easily used in loops, which terminate upon reaching the
 - top, the parent of / is "" -}
parentDir :: FilePath -> FilePath
parentDir dir
	| null dirs = ""
	| otherwise = joinDrive drive (join s $ init dirs)
  where
	-- on Unix, the drive will be "/" when the dir is absolute, otherwise ""
	(drive, path) = splitDrive dir
	dirs = filter (not . null) $ split s path
	s = [pathSeparator]

prop_parentDir_basics :: FilePath -> Bool
prop_parentDir_basics dir
	| null dir = True
	| dir == "/" = parentDir dir == ""
	| otherwise = p /= dir
  where
	p = parentDir dir

{- Checks if the first FilePath is, or could be said to contain the second.
 - For example, "foo/" contains "foo/bar". Also, "foo", "./foo", "foo/" etc
 - are all equivilant.
 -}
dirContains :: FilePath -> FilePath -> Bool
dirContains a b = a == b || a' == b' || (addTrailingPathSeparator a') `isPrefixOf` b'
  where
	a' = norm a
	b' = norm b
	norm = normalise . simplifyPath

{- Converts a filename into an absolute path.
 -
 - Unlike Directory.canonicalizePath, this does not require the path
 - already exists. -}
absPath :: FilePath -> IO FilePath
absPath file = do
	cwd <- getCurrentDirectory
	return $ absPathFrom cwd file

{- Constructs a relative path from the CWD to a file.
 -
 - For example, assuming CWD is /tmp/foo/bar:
 -    relPathCwdToFile "/tmp/foo" == ".."
 -    relPathCwdToFile "/tmp/foo/bar" == "" 
 -}
relPathCwdToFile :: FilePath -> IO FilePath
relPathCwdToFile f = relPathDirToFile <$> getCurrentDirectory <*> absPath f

{- Constructs a relative path from a directory to a file.
 -
 - Both must be absolute, and cannot contain .. etc. (eg use absPath first).
 -}
relPathDirToFile :: FilePath -> FilePath -> FilePath
relPathDirToFile from to = join s $ dotdots ++ uncommon
  where
	s = [pathSeparator]
	pfrom = split s from
	pto = split s to
	common = map fst $ takeWhile same $ zip pfrom pto
	same (c,d) = c == d
	uncommon = drop numcommon pto
	dotdots = replicate (length pfrom - numcommon) ".."
	numcommon = length common

prop_relPathDirToFile_basics :: FilePath -> FilePath -> Bool
prop_relPathDirToFile_basics from to
	| from == to = null r
	| otherwise = not (null r)
  where
	r = relPathDirToFile from to 

prop_relPathDirToFile_regressionTest :: Bool
prop_relPathDirToFile_regressionTest = same_dir_shortcurcuits_at_difference
  where
	{- Two paths have the same directory component at the same
	 - location, but it's not really the same directory.
	 - Code used to get this wrong. -}
	same_dir_shortcurcuits_at_difference =
		relPathDirToFile (joinPath [pathSeparator : "tmp", "r", "lll", "xxx", "yyy", "18"])
			(joinPath [pathSeparator : "tmp", "r", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"])
				== joinPath ["..", "..", "..", "..", ".git", "annex", "objects", "18", "gk", "SHA256-foo", "SHA256-foo"]

{- Given an original list of paths, and an expanded list derived from it,
 - generates a list of lists, where each sublist corresponds to one of the
 - original paths. When the original path is a directory, any items
 - in the expanded list that are contained in that directory will appear in
 - its segment.
 -}
segmentPaths :: [FilePath] -> [FilePath] -> [[FilePath]]
segmentPaths [] new = [new]
segmentPaths [_] new = [new] -- optimisation
segmentPaths (l:ls) new = [found] ++ segmentPaths ls rest
  where
	(found, rest)=partition (l `dirContains`) new

{- This assumes that it's cheaper to call segmentPaths on the result,
 - than it would be to run the action separately with each path. In
 - the case of git file list commands, that assumption tends to hold.
 -}
runSegmentPaths :: ([FilePath] -> IO [FilePath]) -> [FilePath] -> IO [[FilePath]]
runSegmentPaths a paths = segmentPaths paths <$> a paths

{- Converts paths in the home directory to use ~/ -}
relHome :: FilePath -> IO String
relHome path = do
	home <- myHomeDir
	return $ if dirContains home path
		then "~/" ++ relPathDirToFile home path
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
 -}
searchPath :: String -> IO (Maybe FilePath)
searchPath command
	| isAbsolute command = check command
	| otherwise = getSearchPath >>= getM indir
  where
	indir d = check $ d </> command
	check f = firstM doesFileExist
#ifdef mingw32_HOST_OS
		[f, f ++ ".exe"]
#else
		[f]
#endif

{- Checks if a filename is a unix dotfile. All files inside dotdirs
 - count as dotfiles. -}
dotfile :: FilePath -> Bool
dotfile file
	| f == "." = False
	| f == ".." = False
	| f == "" = False
	| otherwise = "." `isPrefixOf` f || dotfile (takeDirectory file)
  where
	f = takeFileName file

{- Converts a DOS style path to a Cygwin style path. Only on Windows.
 - Any trailing '\' is preserved as a trailing '/' -}
toCygPath :: FilePath -> FilePath
#ifndef mingw32_HOST_OS
toCygPath = id
#else
toCygPath p
	| null drive = recombine parts
	| otherwise = recombine $ "/cygdrive" : driveletter drive : parts
  where
  	(drive, p') = splitDrive p
	parts = splitDirectories p'
  	driveletter = map toLower . takeWhile (/= ':')
	recombine = fixtrailing . Posix.joinPath
  	fixtrailing s
		| hasTrailingPathSeparator p = Posix.addTrailingPathSeparator s
		| otherwise = s
#endif

{- Maximum size to use for a file in a specified directory.
 -
 - Many systems have a 255 byte limit to the name of a file, 
 - so that's taken as the max if the system has a larger limit, or has no
 - limit.
 -}
fileNameLengthLimit :: FilePath -> IO Int
#ifdef mingw32_HOST_OS
fileNameLengthLimit _ = return 255
#else
fileNameLengthLimit dir = do
	l <- fromIntegral <$> getPathVar dir FileNameLimit
	if l <= 0
		then return 255
		else return $ minimum [l, 255]
  where
#endif

{- Given a string that we'd like to use as the basis for FilePath, but that
 - was provided by a third party and is not to be trusted, returns the closest
 - sane FilePath.
 -
 - All spaces and punctuation and other wacky stuff are replaced
 - with '_', except for '.' "../" will thus turn into ".._", which is safe.
 -}
sanitizeFilePath :: String -> FilePath
sanitizeFilePath = map sanitize
  where
  	sanitize c
		| c == '.' = c
		| isSpace c || isPunctuation c || isSymbol c || isControl c || c == '/' = '_'
		| otherwise = c

{- Similar to splitExtensions, but knows that some things in FilePaths
 - after a dot are too long to be extensions. -}
splitShortExtensions :: FilePath -> (FilePath, [String])
splitShortExtensions = splitShortExtensions' 5 -- enough for ".jpeg"
splitShortExtensions' :: Int -> FilePath -> (FilePath, [String])
splitShortExtensions' maxextension = go []
  where
	go c f
		| len > 0 && len <= maxextension && not (null base) = 
			go (ext:c) base
		| otherwise = (f, c)
	  where
		(base, ext) = splitExtension f
		len = length ext

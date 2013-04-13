{- path manipulation
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Path where

import Data.String.Utils
import "MissingH" System.Path
import System.FilePath
import System.Directory
import Data.List
import Data.Maybe
import Control.Applicative

import Utility.Monad
import Utility.UserInfo

{- Returns the parent directory of a path. Parent of / is "" -}
parentDir :: FilePath -> FilePath
parentDir dir
	| not $ null dirs = slash ++ join s (init dirs)
	| otherwise = ""
  where
	dirs = filter (not . null) $ split s dir
	slash = if isAbsolute dir then s else ""
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
dirContains a b = a == b || a' == b' || (a'++"/") `isPrefixOf` b'
  where
	norm p = fromMaybe "" $ absNormPath p "."
	a' = norm a
	b' = norm b

{- Converts a filename into a normalized, absolute path.
 -
 - Unlike Directory.canonicalizePath, this does not require the path
 - already exists. -}
absPath :: FilePath -> IO FilePath
absPath file = do
	cwd <- getCurrentDirectory
	return $ absPathFrom cwd file

{- Converts a filename into a normalized, absolute path
 - from the specified cwd. -}
absPathFrom :: FilePath -> FilePath -> FilePath
absPathFrom cwd file = fromMaybe bad $ absNormPath cwd file
  where
	bad = error $ "unable to normalize " ++ file

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
 - Both must be absolute, and normalized (eg with absNormpath).
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
		relPathDirToFile "/tmp/r/lll/xxx/yyy/18" "/tmp/r/.git/annex/objects/18/gk/SHA256-foo/SHA256-foo" == "../../../../.git/annex/objects/18/gk/SHA256-foo/SHA256-foo"

{- Given an original list of paths, and an expanded list derived from it,
 - generates a list of lists, where each sublist corresponds to one of the
 - original paths. When the original path is a direcotry, any items
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
	check f = ifM (doesFileExist f) ( return (Just f), return Nothing )

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

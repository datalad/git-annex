{- path manipulation
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Path where

import Data.String.Utils
import System.Path
import System.FilePath
import System.Directory
import Data.List
import Data.Maybe
import Control.Applicative
import System.Posix.User

import Utility.Monad

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
		common = map fst $ filter same $ zip pfrom pto
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

{- Given an original list of files, and an expanded list derived from it,
 - ensures that the original list's ordering is preserved. 
 -
 - The input list may contain a directory, like "dir" or "dir/". Any
 - items in the expanded list that are contained in that directory will
 - appear at the same position as it did in the input list.
 -}
preserveOrder :: [FilePath] -> [FilePath] -> [FilePath]
preserveOrder [] new = new
preserveOrder [_] new = new -- optimisation
preserveOrder (l:ls) new = found ++ preserveOrder ls rest
	where
		(found, rest)=partition (l `dirContains`) new

{- Runs an action that takes a list of FilePaths, and ensures that 
 - its return list preserves order.
 -
 - This assumes that it's cheaper to call preserveOrder on the result,
 - than it would be to run the action separately with each param. In the case
 - of git file list commands, that assumption tends to hold.
 -}
runPreserveOrder :: ([FilePath] -> IO [FilePath]) -> [FilePath] -> IO [FilePath]
runPreserveOrder a files = preserveOrder files <$> a files

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: FilePath -> IO [FilePath]
dirContents d = map (d </>) . filter notcruft <$> getDirectoryContents d
	where
		notcruft "." = False
		notcruft ".." = False
		notcruft _ = True

{- Current user's home directory. -}
myHomeDir :: IO FilePath
myHomeDir = homeDirectory <$> (getUserEntryForID =<< getEffectiveUserID)

{- Checks if a command is available in PATH. -}
inPath :: String -> IO Bool
inPath command = getSearchPath >>= anyM indir
	where
		indir d = doesFileExist $ d </> command

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

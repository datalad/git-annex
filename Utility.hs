{- general purpose utility functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility (
	CommandParam(..),
	toCommand,
	hGetContentsStrict,
	readFileStrict,
	parentDir,
	absPath,
	absPathFrom,
	relPathCwdToFile,
	relPathDirToFile,
	boolSystem,
	boolSystemEnv,
	executeFile,
	shellEscape,
	shellUnEscape,
	unsetFileMode,
	readMaybe,
	viaTmp,
	dirContains,
	dirContents,
	myHomeDir,
	catchBool,
	whenM,
	(>>?),
	unlessM,
	(>>!),
	
	prop_idempotent_shellEscape,
	prop_idempotent_shellEscape_multiword,
	prop_parentDir_basics,
	prop_relPathDirToFile_basics
) where

import System.IO
import System.Exit
import qualified System.Posix.Process
import System.Posix.Process hiding (executeFile)
import System.Posix.Signals
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import Data.String.Utils
import System.Path
import System.FilePath
import System.Directory
import Foreign (complement)
import Data.List
import Data.Maybe
import Control.Monad (liftM2, when, unless)
import System.Log.Logger

{- A type for parameters passed to a shell command. A command can
 - be passed either some Params (multiple parameters can be included,
 - whitespace-separated, or a single Param (for when parameters contain
 - whitespace), or a File.
 -}
data CommandParam = Params String | Param String | File FilePath
	deriving (Eq, Show, Ord)

{- Used to pass a list of CommandParams to a function that runs
 - a command and expects Strings. -}
toCommand :: [CommandParam] -> [String]
toCommand = (>>= unwrap)
	where
		unwrap (Param s) = [s]
		unwrap (Params s) = filter (not . null) (split " " s)
		-- Files that start with a dash are modified to avoid
		-- the command interpreting them as options.
		unwrap (File ('-':s)) = ["./-" ++ s]
		unwrap (File s) = [s]

{- Run a system command, and returns True or False
 - if it succeeded or failed.
 -
 - SIGINT(ctrl-c) is allowed to propigate and will terminate the program.
 -}
boolSystem :: FilePath -> [CommandParam] -> IO Bool
boolSystem command params = boolSystemEnv command params Nothing

boolSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
boolSystemEnv command params env = do
	-- Going low-level because all the high-level system functions
	-- block SIGINT etc. We need to block SIGCHLD, but allow
	-- SIGINT to do its default program termination.
	let sigset = addSignal sigCHLD emptySignalSet
	oldint <- installHandler sigINT Default Nothing
	oldset <- getSignalMask
	blockSignals sigset
	childpid <- forkProcess $ childaction oldint oldset
	mps <- getProcessStatus True False childpid
	restoresignals oldint oldset
	case mps of
		Just (Exited ExitSuccess) -> return True
		_ -> return False
	where
		restoresignals oldint oldset = do
			_ <- installHandler sigINT oldint Nothing
			setSignalMask oldset
		childaction oldint oldset = do
			restoresignals oldint oldset
			executeFile command True (toCommand params) env

{- executeFile with debug logging -}
executeFile :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ()
executeFile c path p e = do
	debugM "Utility.executeFile" $
		"Running: " ++ c ++ " " ++ show p ++ " " ++ maybe "" show e
	System.Posix.Process.executeFile c path p e

{- Escapes a filename or other parameter to be safely able to be exposed to
 - the shell. -}
shellEscape :: String -> String
shellEscape f = "'" ++ escaped ++ "'"
	where
		-- replace ' with '"'"'
		escaped = join "'\"'\"'" $ split "'" f

{- Unescapes a set of shellEscaped words or filenames. -}
shellUnEscape :: String -> [String]
shellUnEscape [] = []
shellUnEscape s = word : shellUnEscape rest
	where
		(word, rest) = findword "" s
		findword w [] = (w, "")
		findword w (c:cs)
			| c == ' ' = (w, cs)
			| c == '\'' = inquote c w cs
			| c == '"' = inquote c w cs
			| otherwise = findword (w++[c]) cs
		inquote _ w [] = (w, "")
		inquote q w (c:cs)
			| c == q = findword w cs
			| otherwise = inquote q (w++[c]) cs

{- For quickcheck. -}
prop_idempotent_shellEscape :: String -> Bool
prop_idempotent_shellEscape s = [s] == (shellUnEscape . shellEscape) s
prop_idempotent_shellEscape_multiword :: [String] -> Bool
prop_idempotent_shellEscape_multiword s = s == (shellUnEscape . unwords . map shellEscape) s

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h  = hGetContents h >>= \s -> length s `seq` return s

{- A version of readFile that is not lazy. -}
readFileStrict :: FilePath -> IO String
readFileStrict f = readFile f >>= \s -> length s `seq` return s

{- Returns the parent directory of a path. Parent of / is "" -}
parentDir :: FilePath -> FilePath
parentDir dir =
	if not $ null dirs
	then slash ++ join s (take (length dirs - 1) dirs)
	else ""
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

{- Converts a filename into a normalized, absolute path. -}
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
relPathCwdToFile f = liftM2 relPathDirToFile getCurrentDirectory (absPath f)

{- Constructs a relative path from a directory to a file.
 -
 - Both must be absolute, and normalized (eg with absNormpath).
 -}
relPathDirToFile :: FilePath -> FilePath -> FilePath
relPathDirToFile from to = path
	where
		s = [pathSeparator]
		pfrom = split s from
		pto = split s to
		common = map fst $ filter same $ zip pfrom pto
		same (c,d) = c == d
		uncommon = drop numcommon pto
		dotdots = replicate (length pfrom - numcommon) ".."
		numcommon = length common
		path = join s $ dotdots ++ uncommon

prop_relPathDirToFile_basics :: FilePath -> FilePath -> Bool
prop_relPathDirToFile_basics from to
	| from == to = null r
	| otherwise = not (null r)
	where
		r = relPathDirToFile from to 

{- Removes a FileMode from a file.
 - For example, call with otherWriteMode to chmod o-w -}
unsetFileMode :: FilePath -> FileMode -> IO ()
unsetFileMode f m = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `intersectFileModes` complement m

{- Attempts to read a value from a String. -}
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
	((x,_):_) -> Just x
	_ -> Nothing

{- Runs an action like writeFile, writing to a tmp file first and
 - then moving it into place. -}
viaTmp :: (FilePath -> String -> IO ()) -> FilePath -> String -> IO ()
viaTmp a file content = do
	pid <- getProcessID
        let tmpfile = file ++ ".tmp" ++ show pid
	createDirectoryIfMissing True (parentDir file)
	a tmpfile content
	renameFile tmpfile file

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: FilePath -> IO [FilePath]
dirContents d = do
	c <- getDirectoryContents d
	return $ map (d </>) $ filter notcruft c
	where
		notcruft "." = False
		notcruft ".." = False
		notcruft _ = True

{- Current user's home directory. -}
myHomeDir :: IO FilePath
myHomeDir = do
	uid <- getEffectiveUserID
	u <- getUserEntryForID uid
	return $ homeDirectory u

{- Catches IO errors and returns a Bool -}
catchBool :: IO Bool -> IO Bool
catchBool = flip catch (const $ return False)

{- when with a monadic conditional -}
whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= flip when a

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= flip unless a

(>>?) :: Monad m => m Bool -> m () -> m ()
(>>?) = whenM

(>>!) :: Monad m => m Bool -> m () -> m ()
(>>!) = unlessM

-- low fixity allows eg, foo bar >>! error $ "failed " ++ meep
infixr 0 >>?
infixr 0 >>!

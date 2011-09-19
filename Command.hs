{- git-annex command infrastructure
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command where

import Control.Monad.State (liftIO)
import System.Directory
import System.Posix.Files
import Control.Monad (filterM, liftM, when)
import Control.Applicative
import Data.List
import Data.Maybe

import Types
import qualified Backend
import Messages
import qualified Annex
import qualified Git
import qualified Git.LsFiles as LsFiles
import Utility
import Types.Key
import Trust
import LocationLog
import Config
import Backend
import Limit

{- A command runs in four stages.
 -
 - 0. The seek stage takes the parameters passed to the command,
 -    looks through the repo to find the ones that are relevant
 -    to that command (ie, new files to add), and generates
 -    a list of start stage actions. -}
type CommandSeek = [String] -> Annex [CommandStart]
{- 1. The start stage is run before anything is printed about the
  -   command, is passed some input, and can early abort it
  -   if the input does not make sense. It should run quickly and
  -   should not modify Annex state. -}
type CommandStart = Annex (Maybe CommandPerform)
{- 2. The perform stage is run after a message is printed about the command
 -    being run, and it should be where the bulk of the work happens. -}
type CommandPerform = Annex (Maybe CommandCleanup)
{- 3. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the command. -}
type CommandCleanup = Annex Bool

data Command = Command {
	cmdusesrepo :: Bool,
	cmdname :: String,
	cmdparams :: String,
	cmdseek :: [CommandSeek],
	cmddesc :: String
}

{- Most commands operate on files in a git repo. -}
repoCommand :: String -> String -> [CommandSeek] -> String -> Command
repoCommand = Command True

{- Others can run anywhere. -}
standaloneCommand :: String -> String -> [CommandSeek] -> String -> Command
standaloneCommand = Command False

{- For start and perform stages to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- Or to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Prepares a list of actions to run to perform a command, based on
 - the parameters passed to it. -}
prepCommand :: Command -> [String] -> Annex [Annex Bool]
prepCommand Command { cmdseek = seek } params = do
	lists <- mapM (\s -> s params) seek
	return $ map doCommand $ concat lists

{- Runs a command through the start, perform and cleanup stages -}
doCommand :: CommandStart -> CommandCleanup
doCommand = start
	where
		start   = stage $ maybe success perform
		perform = stage $ maybe failure cleanup
		cleanup = stage $ \r -> showEndResult r >> return r
		stage a b = b >>= a
		success = return True
		failure = do
			showEndFail
			return False

notAnnexed :: FilePath -> Annex (Maybe a) -> Annex (Maybe a)
notAnnexed file a = maybe a (const $ return Nothing) =<< Backend.lookupFile file

isAnnexed :: FilePath -> ((Key, Backend Annex) -> Annex (Maybe a)) -> Annex (Maybe a)
isAnnexed file a = maybe (return Nothing) a =<< Backend.lookupFile file

notBareRepo :: Annex a -> Annex a
notBareRepo a = do
	g <- Annex.gitRepo
	when (Git.repoIsLocalBare g) $
		error "You cannot run this subcommand in a bare repository."
	a

{- These functions find appropriate files or other things based on a
   user's parameters, and run a specified action on them. -}
withFilesInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesInGit a params = do
	repo <- Annex.gitRepo
	runFiltered a $ liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
withAttrFilesInGit :: String -> ((FilePath, String) -> CommandStart) -> CommandSeek
withAttrFilesInGit attr a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	run $ liftIO $ Git.checkAttr repo attr files
	where
		run fs = do
			matcher <- Limit.getMatcher
			liftM (map $ proc matcher) fs
		proc matcher p@(f, _) = do
			ok <- matcher f
			if ok then a p else stop
withNumCopies :: (FilePath -> Maybe Int -> CommandStart) -> CommandSeek
withNumCopies a params = withAttrFilesInGit "annex.numcopies" go params
	where
		go (file, v) = do
			let numcopies = readMaybe v
			a file numcopies
withBackendFilesInGit :: (BackendFile -> CommandStart) -> CommandSeek
withBackendFilesInGit a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	backendPairs a files
withFilesMissing :: (String -> CommandStart) -> CommandSeek
withFilesMissing a params = runFiltered a $ liftIO $ filterM missing params
	where
		missing = liftM not . doesFileExist
withFilesNotInGit :: (BackendFile -> CommandStart) -> CommandSeek
withFilesNotInGit a params = do
	repo <- Annex.gitRepo
	force <- Annex.getState Annex.force
	newfiles <- liftIO $ runPreserveOrder (LsFiles.notInRepo repo force) params
	backendPairs a newfiles
withWords :: ([String] -> CommandStart) -> CommandSeek
withWords a params = return [a params]
withStrings :: (String -> CommandStart) -> CommandSeek
withStrings a params = return $ map a params
withFilesToBeCommitted :: (String -> CommandStart) -> CommandSeek
withFilesToBeCommitted a params = do
	repo <- Annex.gitRepo
	runFiltered a $
		liftIO $ runPreserveOrder (LsFiles.stagedNotDeleted repo) params
withFilesUnlocked :: (BackendFile -> CommandStart) -> CommandSeek
withFilesUnlocked = withFilesUnlocked' LsFiles.typeChanged
withFilesUnlockedToBeCommitted :: (BackendFile -> CommandStart) -> CommandSeek
withFilesUnlockedToBeCommitted = withFilesUnlocked' LsFiles.typeChangedStaged
withFilesUnlocked' :: (Git.Repo -> [FilePath] -> IO [FilePath]) -> (BackendFile -> CommandStart) -> CommandSeek
withFilesUnlocked' typechanged a params = do
	-- unlocked files have changed type from a symlink to a regular file
	repo <- Annex.gitRepo
	typechangedfiles <- liftIO $ runPreserveOrder (typechanged repo) params
	unlockedfiles <- liftIO $ filterM notSymlink $
		map (\f -> Git.workTree repo ++ "/" ++ f) typechangedfiles
	backendPairs a unlockedfiles
withKeys :: (Key -> CommandStart) -> CommandSeek
withKeys a params = return $ map (a . parse) params
	where
		parse p = fromMaybe (error "bad key") $ readKey p
withNothing :: CommandStart -> CommandSeek
withNothing a [] = return [a]
withNothing _ _ = error "This command takes no parameters."

runFiltered :: (FilePath -> Annex (Maybe a)) -> Annex [FilePath] -> Annex [Annex (Maybe a)]
runFiltered a fs = do
	matcher <- Limit.getMatcher
	liftM (map $ proc matcher) fs
	where
		proc matcher f = do
			ok <- matcher f
			if ok then a f else stop

backendPairs :: (BackendFile -> CommandStart) -> CommandSeek
backendPairs a fs = do
	matcher <- Limit.getMatcher
	liftM (map $ proc matcher) (Backend.chooseBackends fs)
	where
		proc matcher p@(_, f) = do
			ok <- matcher f
			if ok then a p else stop

{- filter out symlinks -}	
notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f

{- Descriptions of params used in usage messages. -}
paramPaths :: String
paramPaths = paramOptional $ paramRepeating paramPath -- most often used
paramPath :: String
paramPath = "PATH"
paramKey :: String
paramKey = "KEY"
paramDesc :: String
paramDesc = "DESC"
paramUrl :: String
paramUrl = "URL"
paramNumber :: String
paramNumber = "NUMBER"
paramRemote :: String
paramRemote = "REMOTE"
paramGlob :: String
paramGlob = "GLOB"
paramName :: String
paramName = "NAME"
paramType :: String
paramType = "TYPE"
paramKeyValue :: String
paramKeyValue = "K=V"
paramNothing :: String
paramNothing = ""
paramRepeating :: String -> String
paramRepeating s = s ++ " ..."
paramOptional :: String -> String
paramOptional s = "[" ++ s ++ "]"
paramPair :: String -> String -> String
paramPair a b = a ++ " " ++ b

{- The Key specified by the --key parameter. -}
cmdlineKey :: Annex Key
cmdlineKey  = do
	k <- Annex.getState Annex.defaultkey
	case k of
		Nothing -> nokey
		Just "" -> nokey
		Just kstring -> maybe badkey return $ readKey kstring
	where
		nokey = error "please specify the key with --key"
		badkey = error "bad key"

{- Given an original list of files, and an expanded list derived from it,
 - ensures that the original list's ordering is preserved. 
 -
 - The input list may contain a directory, like "dir" or "dir/". Any
 - items in the expanded list that are contained in that directory will
 - appear at the same position as it did in the input list.
 -}
preserveOrder :: [FilePath] -> [FilePath] -> [FilePath]
-- optimisation, only one item in original list, so no reordering needed
preserveOrder [_] new = new
preserveOrder orig new = collect orig new
	where
		collect [] n = n
		collect [_] n = n -- optimisation
		collect (l:ls) n = found ++ collect ls rest
			where (found, rest)=partition (l `dirContains`) n

{- Runs an action that takes a list of FilePaths, and ensures that 
 - its return list preserves order.
 -
 - This assumes that it's cheaper to call preserveOrder on the result,
 - than it would be to run the action separately with each param. In the case
 - of git file list commands, that assumption tends to hold.
 -}
runPreserveOrder :: ([FilePath] -> IO [FilePath]) -> [FilePath] -> IO [FilePath]
runPreserveOrder a files = preserveOrder files <$> a files

{- Used for commands that have an auto mode that checks the number of known
 - copies of a key.
 -
 - In auto mode, first checks that the number of known
 - copies of the key is > or < than the numcopies setting, before running
 - the action. -}
autoCopies :: Key -> (Int -> Int -> Bool) -> Maybe Int -> CommandStart -> CommandStart
autoCopies key vs numcopiesattr a = do
	auto <- Annex.getState Annex.auto
	if auto
		then do
			needed <- getNumCopies numcopiesattr
			(_, have) <- trustPartition UnTrusted =<< keyLocations key
			if length have `vs` needed then a else stop
		else a

{- git-annex commands
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
import System.Path.WildMatch
import Text.Regex.PCRE.Light.Char8
import Data.List
import Data.Maybe
import Data.String.Utils

import Types
import qualified Backend
import Messages
import qualified Annex
import qualified Git
import qualified Git.LsFiles as LsFiles
import Utility
import Types.Key

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
{- Some helper functions are used to build up CommandSeek and CommandStart
 - functions. -}
type CommandSeekStrings = CommandStartString -> CommandSeek
type CommandStartString = String -> CommandStart
type CommandSeekWords = CommandStartWords -> CommandSeek
type CommandStartWords = [String] -> CommandStart
type CommandSeekKeys = CommandStartKey -> CommandSeek
type CommandStartKey = Key -> CommandStart
type BackendFile = (FilePath, Maybe (Backend Annex))
type CommandSeekBackendFiles = CommandStartBackendFile -> CommandSeek
type CommandStartBackendFile = BackendFile -> CommandStart
type AttrFile = (FilePath, String)
type CommandSeekAttrFiles = CommandStartAttrFile -> CommandSeek
type CommandStartAttrFile = AttrFile -> CommandStart
type CommandSeekNothing = CommandStart -> CommandSeek
type CommandStartNothing = CommandStart

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
			showOutput -- avoid clutter around error message
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
withFilesInGit :: CommandSeekStrings
withFilesInGit a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	liftM (map a) $ filterFiles files
withAttrFilesInGit :: String -> CommandSeekAttrFiles
withAttrFilesInGit attr a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	liftM (map a) $ liftIO $ Git.checkAttr repo attr files
withBackendFilesInGit :: CommandSeekBackendFiles
withBackendFilesInGit a params = do
	repo <- Annex.gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	files' <- filterFiles files
	backendPairs a files'
withFilesMissing :: CommandSeekStrings
withFilesMissing a params = do
	files <- liftIO $ filterM missing params
	liftM (map a) $ filterFiles files
	where
		missing f = do
			e <- doesFileExist f
			return $ not e
withFilesNotInGit :: CommandSeekBackendFiles
withFilesNotInGit a params = do
	repo <- Annex.gitRepo
	force <- Annex.getState Annex.force
	newfiles <- liftIO $ runPreserveOrder (LsFiles.notInRepo repo force) params
	newfiles' <- filterFiles newfiles
	backendPairs a newfiles'
withWords :: CommandSeekWords
withWords a params = return [a params]
withStrings :: CommandSeekStrings
withStrings a params = return $ map a params
withFilesToBeCommitted :: CommandSeekStrings
withFilesToBeCommitted a params = do
	repo <- Annex.gitRepo
	tocommit <- liftIO $ runPreserveOrder (LsFiles.stagedNotDeleted repo) params
	liftM (map a) $ filterFiles tocommit
withFilesUnlocked :: CommandSeekBackendFiles
withFilesUnlocked = withFilesUnlocked' LsFiles.typeChanged
withFilesUnlockedToBeCommitted :: CommandSeekBackendFiles
withFilesUnlockedToBeCommitted = withFilesUnlocked' LsFiles.typeChangedStaged
withFilesUnlocked' :: (Git.Repo -> [FilePath] -> IO [FilePath]) -> CommandSeekBackendFiles
withFilesUnlocked' typechanged a params = do
	-- unlocked files have changed type from a symlink to a regular file
	repo <- Annex.gitRepo
	typechangedfiles <- liftIO $ runPreserveOrder (typechanged repo) params
	unlockedfiles <- liftIO $ filterM notSymlink $
		map (\f -> Git.workTree repo ++ "/" ++ f) typechangedfiles
	unlockedfiles' <- filterFiles unlockedfiles
	backendPairs a unlockedfiles'
withKeys :: CommandSeekKeys
withKeys a params = return $ map (a . parse) params
	where
		parse p = fromMaybe (error "bad key") $ readKey p
withTempFile :: CommandSeekStrings
withTempFile a params = return $ map a params
withNothing :: CommandSeekNothing
withNothing a [] = return [a]
withNothing _ _ = error "This command takes no parameters."

backendPairs :: CommandSeekBackendFiles
backendPairs a files = liftM (map a) $ Backend.chooseBackends files

{- Filter out files those matching the exclude glob pattern,
 - if it was specified. -}
filterFiles :: [FilePath] -> Annex [FilePath]
filterFiles l = do
	exclude <- Annex.getState Annex.exclude
	if null exclude
		then return l
		else return $ filter (notExcluded $ wildsRegex exclude) l
	where
		notExcluded r f = isNothing $ match r f []

wildsRegex :: [String] -> Regex
wildsRegex ws = compile regex []
	where
		regex = "^(" ++ alternatives ++ ")"
		alternatives = join "|" $ map wildToRegex ws

{- filter out symlinks -}	
notSymlink :: FilePath -> IO Bool
notSymlink f = liftM (not . isSymbolicLink) $ liftIO $ getSymbolicLinkStatus f

{- Descriptions of params used in usage messages. -}
paramRepeating :: String -> String
paramRepeating s = s ++ " ..."
paramOptional :: String -> String
paramOptional s = "[" ++ s ++ "]"
paramPair :: String -> String -> String
paramPair a b = a ++ " " ++ b
paramPath :: String
paramPath = "PATH"
paramKey :: String
paramKey = "KEY"
paramDesc :: String
paramDesc = "DESC"
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
runPreserveOrder a files = liftM (preserveOrder files) (a files)

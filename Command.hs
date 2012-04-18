{- git-annex command infrastructure
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command (
	command,
	noRepo,
	oneShot,
	withOptions,
	next,
	stop,
	stopUnless,
	prepCommand,
	doCommand,
	whenAnnexed,
	ifAnnexed,
	notBareRepo,
	isBareRepo,
	numCopies,
	autoCopies,
	module ReExported
) where

import Common.Annex
import qualified Backend
import qualified Annex
import qualified Git
import qualified Remote
import Types.Command as ReExported
import Types.Option as ReExported
import Seek as ReExported
import Checks as ReExported
import Usage as ReExported
import Logs.Trust
import Config
import Annex.CheckAttr

{- Generates a normal command -}
command :: String -> String -> [CommandSeek] -> String -> Command
command = Command [] Nothing commonChecks False

{- Makes a command run in oneshot mode. -}
oneShot :: Command -> Command
oneShot c = c { cmdoneshot = True }

{- Adds a fallback action to a command, that will be run if it's used
 - outside a git repository. -}
noRepo :: IO () -> Command -> Command
noRepo a c = c { cmdnorepo = Just a }

{- Adds options to a command. -}
withOptions :: [Option] -> Command -> Command
withOptions o c = c { cmdoptions = o }

{- For start and perform stages to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- Or to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Stops unless a condition is met. -}
stopUnless :: Annex Bool -> Annex (Maybe a) -> Annex (Maybe a)
stopUnless c a = ifM c ( a , stop )

{- Prepares to run a command via the check and seek stages, returning a
 - list of actions to perform to run the command. -}
prepCommand :: Command -> [String] -> Annex [CommandCleanup]
prepCommand Command { cmdseek = seek, cmdcheck = c } params = do
	mapM_ runCheck c
	map doCommand . concat <$> mapM (\s -> s params) seek

{- Runs a command through the start, perform and cleanup stages -}
doCommand :: CommandStart -> CommandCleanup
doCommand = start
	where
		start   = stage $ maybe skip perform
		perform = stage $ maybe failure cleanup
		cleanup = stage $ status
		stage = (=<<)
		skip = return True
		failure = showEndFail >> return False
		status r = showEndResult r >> return r

{- Modifies an action to only act on files that are already annexed,
 - and passes the key and backend on to it. -}
whenAnnexed :: (FilePath -> (Key, Backend) -> Annex (Maybe a)) -> FilePath -> Annex (Maybe a)
whenAnnexed a file = ifAnnexed file (a file) (return Nothing)

ifAnnexed :: FilePath -> ((Key, Backend) -> Annex a) -> Annex a -> Annex a
ifAnnexed file yes no = maybe no yes =<< Backend.lookupFile file

notBareRepo :: Annex a -> Annex a
notBareRepo a = do
	whenM isBareRepo $
		error "You cannot run this subcommand in a bare repository."
	a

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare

numCopies :: FilePath  -> Annex (Maybe Int)
numCopies file = readish <$> checkAttr "annex.numcopies" file

{- Used for commands that have an auto mode that checks the number of known
 - copies of a key.
 -
 - In auto mode, first checks that the number of known
 - copies of the key is > or < than the numcopies setting, before running
 - the action. -}
autoCopies :: FilePath -> Key -> (Int -> Int -> Bool) -> (Maybe Int -> CommandStart) -> CommandStart
autoCopies file key vs a = do
	numcopiesattr <- numCopies file
	Annex.getState Annex.auto >>= auto numcopiesattr
	where
		auto numcopiesattr False = a numcopiesattr
		auto numcopiesattr True = do
			needed <- getNumCopies numcopiesattr
			(_, have) <- trustPartition UnTrusted =<< Remote.keyLocations key
			if length have `vs` needed then a numcopiesattr else stop

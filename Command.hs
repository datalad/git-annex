{- git-annex command infrastructure
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command (
	command,
	noRepo,
	noCommit,
	noMessages,
	withOptions,
	next,
	stop,
	stopUnless,
	prepCommand,
	doCommand,
	whenAnnexed,
	ifAnnexed,
	isBareRepo,
	numCopies,
	numCopiesCheck,
	checkAuto,
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
command :: String -> String -> [CommandSeek] -> CommandSection -> String -> Command
command = Command [] Nothing commonChecks False False

{- Indicates that a command doesn't need to commit any changes to
 - the git-annex branch. -}
noCommit :: Command -> Command
noCommit c = c { cmdnocommit = True }

{- Indicates that a command should not output anything other than what
 - it directly sends to stdout. (--json can override this). -}
noMessages :: Command -> Command
noMessages c = c { cmdnomessages = True }

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

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare

numCopies :: FilePath  -> Annex (Maybe Int)
numCopies file = do
	forced <- Annex.getState Annex.forcenumcopies
	case forced of
		Just n -> return $ Just n
		Nothing -> readish <$> checkAttr "annex.numcopies" file

numCopiesCheck :: FilePath -> Key -> (Int -> Int -> v) -> Annex v
numCopiesCheck file key vs = do
	numcopiesattr <- numCopies file
	needed <- getNumCopies numcopiesattr
	have <- trustExclude UnTrusted =<< Remote.keyLocations key
	return $ length have `vs` needed

checkAuto :: Annex Bool -> Annex Bool
checkAuto checker = ifM (Annex.getState Annex.auto)
	( checker , return True )

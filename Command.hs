{- git-annex command infrastructure
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command (
	module Types.Command,
	module Seek,
	module Checks,
	module Options,
	command,
	next,
	stop,
	prepCommand,
	doCommand,
	whenAnnexed,
	notAnnexed,
	notBareRepo,
	isBareRepo,
	autoCopies
) where

import Common.Annex
import qualified Backend
import qualified Annex
import qualified Git
import Types.Command
import Logs.Trust
import Logs.Location
import Config
import Seek
import Checks
import Options

{- Generates a command with the common checks. -}
command :: String -> String -> [CommandSeek] -> String -> Command
command = Command commonChecks

{- For start and perform stages to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- Or to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Prepares to run a command via the check and seek stages, returning a
 - list of actions to perform to run the command. -}
prepCommand :: Command -> [String] -> Annex [CommandCleanup]
prepCommand Command { cmdseek = seek, cmdcheck = c } params = do
	sequence_ $ map runCheck c
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
whenAnnexed :: (FilePath -> (Key, Backend Annex) -> Annex (Maybe a)) -> FilePath -> Annex (Maybe a)
whenAnnexed a file = maybe (return Nothing) (a file) =<< Backend.lookupFile file

notAnnexed :: FilePath -> Annex (Maybe a) -> Annex (Maybe a)
notAnnexed file a = maybe a (const $ return Nothing) =<< Backend.lookupFile file

notBareRepo :: Annex a -> Annex a
notBareRepo a = do
	whenM isBareRepo $
		error "You cannot run this subcommand in a bare repository."
	a

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare

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

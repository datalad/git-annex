{- git-annex command checks
 -
 - Common sanity checks for commands, and an interface to selectively
 - remove them, or add others.
 - 
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Checks where

import Annex.Common
import Types.Command
import Annex.Init
import Config
import Utility.Daemon
import qualified Git

commonChecks :: [CommandCheck]
commonChecks = [repoExists]

repoExists :: CommandCheck
repoExists = CommandCheck 0 ensureInitialized

notDirect :: Command -> Command
notDirect = addCheck $ whenM isDirect $
	error "You cannot run this command in a direct mode repository."

notBareRepo :: Command -> Command
notBareRepo = addCheck $ whenM (fromRepo Git.repoIsLocalBare) $
	error "You cannot run this command in a bare repository."

noDaemonRunning :: Command -> Command
noDaemonRunning = addCheck $ whenM (isJust <$> daemonpid) $
	error "You cannot run this command while git-annex watch or git-annex assistant is running."
  where
	daemonpid = liftIO . checkDaemon =<< fromRepo gitAnnexPidFile

dontCheck :: CommandCheck -> Command -> Command
dontCheck check cmd = mutateCheck cmd $ \c -> filter (/= check) c

addCheck :: Annex () -> Command -> Command
addCheck check cmd = mutateCheck cmd $ \c ->
	CommandCheck (length c + 100) check : c

mutateCheck :: Command -> ([CommandCheck] -> [CommandCheck]) -> Command
mutateCheck cmd@(Command { cmdcheck = c }) a = cmd { cmdcheck = a c }


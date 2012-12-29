{- git-annex command checks
 -
 - Common sanity checks for commands, and an interface to selectively
 - remove them, or add others.
 - 
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Checks where

import Common.Annex
import Types.Command
import Init
import Config
import qualified Git

commonChecks :: [CommandCheck]
commonChecks = [repoExists]

repoExists :: CommandCheck
repoExists = CommandCheck 0 ensureInitialized

notDirect :: Command -> Command
notDirect = addCheck $ whenM isDirect $
	error "You cannot run this subcommand in a direct mode repository."

notBareRepo :: Command -> Command
notBareRepo = addCheck $ whenM (fromRepo Git.repoIsLocalBare) $
	error "You cannot run this subcommand in a bare repository."

dontCheck :: CommandCheck -> Command -> Command
dontCheck check cmd = mutateCheck cmd $ \c -> filter (/= check) c

addCheck :: Annex () -> Command -> Command
addCheck check cmd = mutateCheck cmd $ \c ->
	CommandCheck (length c + 100) check : c

mutateCheck :: Command -> ([CommandCheck] -> [CommandCheck]) -> Command
mutateCheck cmd@(Command { cmdcheck = c }) a = cmd { cmdcheck = a c }


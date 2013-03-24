{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Test where

import Command

def :: [Command]
def = [ dontCheck repoExists $
	command "test" paramNothing seek SectionPlumbing
		"run built-in test suite"]

seek :: [CommandSeek]
seek = [withWords start]

{- We don't actually run the test suite here because of a dependency loop.
 - The main program notices when the command is test and runs it; this
 - function is never run if that works. -}
start :: [String] -> CommandStart
start _ = error "Cannot specify any additional parameters when running test"

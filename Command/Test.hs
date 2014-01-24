{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Test where

import Common
import Command
import Messages

def :: [Command]
def = [ noRepo startIO $ dontCheck repoExists $
	command "test" paramNothing seek SectionPlumbing
		"run built-in test suite"]

seek :: [CommandSeek]
seek = [withWords start]

{- We don't actually run the test suite here because of a dependency loop.
 - The main program notices when the command is test and runs it; this
 - function is never run if that works.
 -
 - However, if git-annex is built without the test suite, just print a
 - warning, and do not exit nonzero. This is so git-annex test can be run
 - in debian/rules despite some architectures not being able to build the
 - test suite.
 -}
start :: [String] -> CommandStart
start ps = do
	liftIO $ startIO ps
	stop

startIO :: CmdParams -> IO ()
startIO [] = warningIO "git-annex was built without its test suite; not testing"
startIO _ = error "Cannot specify any additional parameters when running test"

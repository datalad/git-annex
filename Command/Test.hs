{- git-annex command
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Test where

import Command
import Types.Test

cmd :: Parser TestOptions -> Maybe TestRunner -> Command
cmd optparser runner = noRepo (startIO runner <$$> const optparser) $
	dontCheck repoExists $
		command "test" SectionTesting
			"run built-in test suite"
			paramNothing (seek runner <$$> const optparser)

seek :: Maybe TestRunner -> TestOptions -> CommandSeek
seek runner o = commandAction $ start runner o

start :: Maybe TestRunner -> TestOptions -> CommandStart
start runner o = do
	liftIO $ startIO runner o
	stop

startIO :: Maybe TestRunner -> TestOptions -> IO ()
startIO Nothing _ = warningIO "git-annex was built without its test suite; not testing"
startIO (Just runner) o = runner o

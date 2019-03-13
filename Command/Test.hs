{- git-annex command
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Test where

import Command
import Types.Test

cmd :: Parser TestOptions -> TestRunner -> Command
cmd optparser runner = noRepo (startIO runner <$$> const optparser) $
	dontCheck repoExists $
		command "test" SectionTesting
			"run built-in test suite"
			paramNothing (seek runner <$$> const optparser)

seek :: TestRunner -> TestOptions -> CommandSeek
seek runner o = commandAction $ start runner o

start :: TestRunner -> TestOptions -> CommandStart
start runner o = do
	liftIO $ startIO runner o
	stop

startIO :: TestRunner -> TestOptions -> IO ()
startIO runner o = runner o

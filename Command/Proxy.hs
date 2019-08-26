{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Proxy where

import Command

cmd :: Command
cmd = notBareRepo $
	command "proxy" SectionPlumbing 
		"safely bypass direct mode guard (deprecated)"
		("-- git command") (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start [] = giveup "Did not specify command to run."
start (c:ps) = liftIO $ exitWith =<< safeSystem c (map Param ps)

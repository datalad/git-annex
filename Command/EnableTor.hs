{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.EnableTor where

import Command
import Utility.Tor

cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	command "enable-tor" SectionPlumbing ""
		paramNumber (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: CmdParams -> CommandStart
start (localport:[]) = case readish localport of
	Nothing -> error "Bad localport"
	Just lp -> do
		(onionaddr, onionport) <- liftIO $ addHiddenService lp
		liftIO $ putStrLn (onionaddr ++ ":" ++ show onionport)
		stop
start _ = error "Need 1 localport parameter"

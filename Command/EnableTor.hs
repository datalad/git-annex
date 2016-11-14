{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.EnableTor where

import Command
import Utility.Tor

-- This runs as root, so avoid making any commits or initializing
-- git-annex, as that would create root-owned files.
cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	command "enable-tor" SectionPlumbing ""
		"userid uuid" (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: CmdParams -> CommandStart
start (suserid:uuid:[]) = case readish suserid of
	Nothing -> error "Bad userid"
	Just userid -> do
		(onionaddr, onionport, onionsocket) <- liftIO $
			addHiddenService userid uuid
		liftIO $ putStrLn $
			onionaddr ++ ":" ++ 
			show onionport ++ " " ++
			show onionsocket
		stop
start _ = error "Bad params"

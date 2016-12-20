{- su to root
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Su where

import Common
import Utility.Env
import Utility.Path

import System.Posix.Terminal

-- Runs a command as root, fairly portably.
--
-- Does not use sudo commands if something else is available, because
-- the user may not be in sudoers and we couldn't differentiate between
-- that and the command failing. Although, some commands like gksu
-- decide based on the system's configuration whether sudo should be used.
runAsRoot :: String -> [CommandParam] -> IO Bool
runAsRoot cmd ps = go =<< firstM (inPath . fst) =<< selectcmds
  where
	go Nothing = return False
	go (Just (cmd', ps')) = boolSystem cmd' ps'

	selectcmds = ifM (inx <||> (not <$> atconsole))
		( return (graphicalcmds ++ consolecmds)
		, return consolecmds
		)
	
	inx = isJust <$> getEnv "DISPLAY"
	atconsole = queryTerminal stdInput

	-- These will only work when the user is logged into a desktop.
	graphicalcmds =
		[ ("gksu", [Param shellcmd])
		, ("kdesu", [Param shellcmd])
		-- Available in Debian's menu package; knows about lots of
		-- ways to gain root.
		, ("su-to-root", [Param "-X", Param "-c", Param shellcmd])
		-- OSX native way to run a command as root, prompts in GUI
		, ("osascript", [Param "-e", Param ("do shell script \"" ++ shellcmd ++ "\" with administrator privileges")])
		]
	
	-- These will only work when run in a console.
	consolecmds = 
		[ ("su", [Param "-c", Param "--", Param cmd] ++ ps)
		, ("sudo", [Param cmd] ++ ps)
		, ("su-to-root", [Param "-c", Param shellcmd])
		]
	
	shellcmd = unwords $ map shellEscape (cmd:toCommand ps)

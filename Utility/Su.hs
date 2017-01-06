{- su to root
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Su where

import Common
import Utility.Env

#ifndef mingw32_HOST_OS
import System.Posix.Terminal
#endif

data WhosePassword
	= RootPassword
	| UserPassword
	| SomePassword
	-- ^ may be user or root; su program should indicate which
	deriving (Show)

data PasswordPrompt
	= WillPromptPassword WhosePassword
	| MayPromptPassword WhosePassword
	| NoPromptPassword
	deriving (Show)

describePasswordPrompt :: PasswordPrompt -> Maybe String
describePasswordPrompt (WillPromptPassword whose) = Just $
	"You will be prompted for " ++ describeWhosePassword whose ++ " password"
describePasswordPrompt (MayPromptPassword whose) = Just $
	"You may be prompted for " ++ describeWhosePassword whose ++ " password"
describePasswordPrompt NoPromptPassword = Nothing

describeWhosePassword :: WhosePassword -> String
describeWhosePassword RootPassword = "root's"
describeWhosePassword UserPassword = "your"
describeWhosePassword SomePassword = "a"

data SuCommand = SuCommand PasswordPrompt String [CommandParam]
	deriving (Show)

describePasswordPrompt' :: Maybe SuCommand -> Maybe String
describePasswordPrompt' (Just (SuCommand p _ _)) = describePasswordPrompt p
describePasswordPrompt' Nothing = Nothing

runSuCommand :: (Maybe SuCommand) -> IO Bool
runSuCommand (Just (SuCommand _ cmd ps)) = boolSystem cmd ps
runSuCommand Nothing = return False

-- Generates a SuCommand that runs a command as root, fairly portably.
--
-- Does not use sudo commands if something else is available, because
-- the user may not be in sudoers and we couldn't differentiate between
-- that and the command failing. Although, some commands like gksu
-- decide based on the system's configuration whether sudo should be used.
mkSuCommand :: String -> [CommandParam] -> IO (Maybe SuCommand)
#ifndef mingw32_HOST_OS
mkSuCommand cmd ps = firstM (\(SuCommand _ p _) -> inPath p) =<< selectcmds
  where
	selectcmds = ifM (inx <||> (not <$> atconsole))
		( return (graphicalcmds ++ consolecmds)
		, return consolecmds
		)
	
	inx = isJust <$> getEnv "DISPLAY"
	atconsole = queryTerminal stdInput

	-- These will only work when the user is logged into a desktop.
	graphicalcmds =
		[ SuCommand (MayPromptPassword SomePassword) "gksu"
			[Param shellcmd]
		, SuCommand (MayPromptPassword SomePassword) "kdesu"
			[Param shellcmd]
		-- Available in Debian's menu package; knows about lots of
		-- ways to gain root.
		, SuCommand (MayPromptPassword SomePassword) "su-to-root"
			[Param "-X", Param "-c", Param shellcmd]
		-- OSX native way to run a command as root, prompts in GUI
		, SuCommand (WillPromptPassword RootPassword) "osascript"
			[Param "-e", Param ("do shell script \"" ++ shellcmd ++ "\" with administrator privileges")]
		]
	
	-- These will only work when run in a console.
	consolecmds = 
		[ SuCommand (WillPromptPassword RootPassword) "su"
			[Param "-c", Param shellcmd]
		, SuCommand (MayPromptPassword UserPassword) "sudo"
			([Param cmd] ++ ps)
		, SuCommand (MayPromptPassword SomePassword) "su-to-root"
			[Param "-c", Param shellcmd]
		]
	
	shellcmd = unwords $ map shellEscape (cmd:toCommand ps)
#else
-- For windows, we assume the user has administrator access.
mkSuCommand cmd ps = return $ Just $ SuCommand NoPromptPassword cmd ps
#endif

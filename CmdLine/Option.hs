{- common command-line options
 -
 - Copyright 2010-2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Option (
	commonOptions,
	flagOption,
	fieldOption,
	optionName,
	optionParam,
	ArgDescr(..),
	OptDescr(..),
) where

import System.Console.GetOpt

import Common.Annex
import qualified Annex
import Types.Messages
import Types.DesktopNotify
import CmdLine.Usage

-- Options accepted by both git-annex and git-annex-shell sub-commands.
commonOptions :: [Option]
commonOptions =
	[ Option [] ["force"] (NoArg (setforce True))
		"allow actions that may lose annexed data"
	, Option ['F'] ["fast"] (NoArg (setfast True))
		"avoid slow operations"
	, Option ['a'] ["auto"] (NoArg (setauto True))
		"automatic mode"
	, Option ['q'] ["quiet"] (NoArg (Annex.setOutput QuietOutput))
		"avoid verbose output"
	, Option ['v'] ["verbose"] (NoArg (Annex.setOutput NormalOutput))
		"allow verbose output (default)"
	, Option ['d'] ["debug"] (NoArg setdebug)
		"show debug messages"
	, Option [] ["no-debug"] (NoArg unsetdebug)
		"don't show debug messages"
	, Option ['b'] ["backend"] (ReqArg setforcebackend paramName)
		"specify key-value backend to use"
	, Option [] ["notify-finish"] (NoArg (setdesktopnotify mkNotifyFinish))
		"show desktop notification after transfer finishes"
	, Option [] ["notify-start"] (NoArg (setdesktopnotify mkNotifyStart))
		"show desktop notification after transfer completes"
	]
  where
	setforce v = Annex.changeState $ \s -> s { Annex.force = v }
	setfast v = Annex.changeState $ \s -> s { Annex.fast = v }
	setauto v = Annex.changeState $ \s -> s { Annex.auto = v }
	setforcebackend v = Annex.changeState $ \s -> s { Annex.forcebackend = Just v }
	setdebug = Annex.changeGitConfig $ \c -> c { annexDebug = True }
	unsetdebug = Annex.changeGitConfig $ \c -> c { annexDebug = False }
	setdesktopnotify v = Annex.changeState $ \s -> s { Annex.desktopnotify = Annex.desktopnotify s <> v }

{- An option that sets a flag. -}
flagOption :: String -> String -> String -> Option
flagOption short opt description = 
	Option short [opt] (NoArg (Annex.setFlag opt)) description

{- An option that sets a field. -}
fieldOption :: String -> String -> String -> String -> Option
fieldOption short opt paramdesc description = 
	Option short [opt] (ReqArg (Annex.setField opt) paramdesc) description

{- The flag or field name used for an option. -}
optionName :: Option -> String
optionName (Option _ o _ _) = Prelude.head o

optionParam :: Option -> String
optionParam o = "--" ++ optionName o

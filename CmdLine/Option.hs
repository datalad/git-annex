{- common command-line options
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Option (
	commonOptions,
	matcherOptions,
	flagOption,
	fieldOption,
	optionName,
	ArgDescr(..),
	OptDescr(..),
) where

import System.Console.GetOpt

import Common.Annex
import qualified Annex
import Types.Messages
import Limit
import CmdLine.Usage

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
	]
  where
	setforce v = Annex.changeState $ \s -> s { Annex.force = v }
	setfast v = Annex.changeState $ \s -> s { Annex.fast = v }
	setauto v = Annex.changeState $ \s -> s { Annex.auto = v }
	setforcebackend v = Annex.changeState $ \s -> s { Annex.forcebackend = Just v }
	setdebug = Annex.changeGitConfig $ \c -> c { annexDebug = True }
	unsetdebug = Annex.changeGitConfig $ \c -> c { annexDebug = False }

matcherOptions :: [Option]
matcherOptions =
	[ longopt "not" "negate next option"
	, longopt "and" "both previous and next option must match"
	, longopt "or" "either previous or next option must match"
	, shortopt "(" "open group of options"
	, shortopt ")" "close group of options"
	]
  where
	longopt o = Option [] [o] $ NoArg $ addToken o
	shortopt o = Option o [] $ NoArg $ addToken o

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

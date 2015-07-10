{- common command-line options
 -
 - Copyright 2010-2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Option (
	commonGlobalOptions,
	flagOption,
	fieldOption,
	optionName,
	optionParam,
	ArgDescr(..),
	OptDescr(..),
) where

import Options.Applicative
import System.Console.GetOpt

import Common.Annex
import CmdLine.Usage
import CmdLine.GlobalSetter
import qualified Annex
import Types.Messages
import Types.DeferredParse

-- Global options accepted by both git-annex and git-annex-shell sub-commands.
commonGlobalOptions :: [Parser GlobalSetter]
commonGlobalOptions =
	[ globalFlag (setforce True)
		( long "force" 
		<> help "allow actions that may lose annexed data"
		<> hidden
		)
	, globalFlag (setfast True)
		( long "fast" <> short 'F'
		<> help "avoid slow operations"
		<> hidden
		)
	, globalFlag (Annex.setOutput QuietOutput)
		( long "quiet" <> short 'q'
		<> help "avoid verbose output"
		<> hidden
		)
	, globalFlag (Annex.setOutput NormalOutput)
		( long "verbose" <> short 'v'
		<> help "allow verbose output (default)"
		<> hidden
		)
	, globalFlag setdebug
		( long "debug" <> short 'd'
		<> help "show debug messages"
		<> hidden
		)
	, globalFlag unsetdebug
		( long "no-debug"
		<> help "don't show debug messages"
		<> hidden
		)
	, globalSetter setforcebackend $ strOption
		( long "backend" <> short 'b' <> metavar paramName
		<> help "specify key-value backend to use"
		<> hidden
		)
	]
  where
	setforce v = Annex.changeState $ \s -> s { Annex.force = v }
	setfast v = Annex.changeState $ \s -> s { Annex.fast = v }
	setforcebackend v = Annex.changeState $ \s -> s { Annex.forcebackend = Just v }
	setdebug = Annex.changeGitConfig $ \c -> c { annexDebug = True }
	unsetdebug = Annex.changeGitConfig $ \c -> c { annexDebug = False }

{- An option that sets a flag. -}
flagOption :: String -> String -> String -> Option
flagOption shortv opt description = 
	Option shortv [opt] (NoArg (Annex.setFlag opt)) description

{- An option that sets a field. -}
fieldOption :: String -> String -> String -> String -> Option
fieldOption shortv opt paramdesc description = 
	Option shortv [opt] (ReqArg (Annex.setField opt) paramdesc) description

{- The flag or field name used for an option. -}
optionName :: Option -> String
optionName (Option _ o _ _) = Prelude.head o

optionParam :: Option -> String
optionParam o = "--" ++ optionName o

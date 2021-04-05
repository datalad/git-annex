{- common command-line options
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module CmdLine.Option where

import Options.Applicative

import CmdLine.Usage
import CmdLine.GlobalSetter
import qualified Annex
import Types.Messages
import Types.DeferredParse
import Types.GitConfig
import Git.Types (ConfigKey(..))
import Git.Config
import Utility.FileSystemEncoding

-- Global options accepted by both git-annex and git-annex-shell sub-commands.
commonGlobalOptions :: [GlobalOption]
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
	, globalFlag (setdebug True)
		( long "debug" <> short 'd'
		<> help "show debug messages"
		<> hidden
		)
	, globalFlag (setdebug False)
		( long "no-debug"
		<> help "don't show debug messages"
		<> hidden
		)
	, globalSetter setdebugfilter $ strOption
		( long "debugfilter" <> metavar "NAME[,NAME..]"
		<> help "show debug messages coming from a module"
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
	-- Overriding this way, rather than just setting annexDebug
	-- makes the config be passed on to any git-annex child processes.
	setdebug v = Annex.addGitConfigOverride $
		decodeBS' $ debugconfig <> "=" <> boolConfig' v
	setdebugfilter v = Annex.addGitConfigOverride $ 
		decodeBS' (debugfilterconfig <> "=") ++ v
	(ConfigKey debugconfig) = annexConfig "debug"
	(ConfigKey debugfilterconfig) = annexConfig "debugfilter"

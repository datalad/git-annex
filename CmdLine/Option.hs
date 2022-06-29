{- common command-line options
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module CmdLine.Option where

import Options.Applicative

import CmdLine.AnnexSetter
import qualified Annex
import Types.Messages
import Types.DeferredParse
import Types.GitConfig
import Git.Types (ConfigKey(..))
import Git.Config
import Utility.FileSystemEncoding
import Annex.Debug

-- Options accepted by both git-annex and git-annex-shell sub-commands.
commonOptions :: [AnnexOption]
commonOptions =
	[ annexFlag (setforce True)
		( long "force" 
		<> help "allow actions that may lose annexed data"
		<> hidden
		)
	, annexFlag (setfast True)
		( long "fast" <> short 'F'
		<> help "avoid slow operations"
		<> hidden
		)
	, annexFlag (setAnnexState $ Annex.setOutput QuietOutput)
		( long "quiet" <> short 'q'
		<> help "avoid verbose output"
		<> hidden
		)
	, annexFlag (setAnnexState $ Annex.setOutput NormalOutput)
		( long "verbose" <> short 'v'
		<> help "allow verbose output (default)"
		<> hidden
		)
	, annexFlag (setdebug True)
		( long "debug" <> short 'd'
		<> help "show debug messages"
		<> hidden
		)
	, annexFlag (setdebug False)
		( long "no-debug"
		<> help "don't show debug messages"
		<> hidden
		)
	, annexOption setdebugfilter $ strOption
		( long "debugfilter" <> metavar "NAME[,NAME..]"
		<> help "show debug messages coming from a module"
		<> hidden
		)
	]
  where
	setforce v = setAnnexRead $ \rd -> rd { Annex.force = v }

	setfast v = setAnnexRead $ \rd -> rd { Annex.fast = v }

	setdebug v = mconcat
		[ setAnnexRead $ \rd -> rd { Annex.debugenabled = v }
		-- Also set in git config so it will be passed on to any
		-- git-annex child processes.
		, setAnnexState $ Annex.addGitConfigOverride $
			decodeBS $ debugconfig <> "=" <> boolConfig' v
		]
	
	setdebugfilter v = mconcat
		[ setAnnexRead $ \rd -> rd
			{ Annex.debugselector = parseDebugSelector v }
		-- Also set in git config so it will be passed on to any
		-- git-annex child processes.
		, setAnnexState $ Annex.addGitConfigOverride $ 
			decodeBS (debugfilterconfig <> "=") ++ v
		]
	
	(ConfigKey debugconfig) = annexConfig "debug"
	(ConfigKey debugfilterconfig) = annexConfig "debugfilter"

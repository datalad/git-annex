{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Config where

import Command
import Logs.Config
import Config

cmd :: Command
cmd = noMessages $ command "config" SectionSetup
	"configuration stored in git-annex branch"
	paramNothing (seek <$$> optParser)

data Action
	= SetConfig ConfigName ConfigValue
	| GetConfig ConfigName
	| UnsetConfig ConfigName

type Name = String
type Value = String

optParser :: CmdParamsDesc -> Parser Action
optParser _ = setconfig <|> getconfig <|> unsetconfig
  where
	setconfig = SetConfig
		<$> strOption
			( long "set"
			<> help "set configuration"
			<> metavar paramName
			)
		<*> strArgument
			( metavar paramValue
			)
	getconfig = GetConfig <$> strOption
		( long "get"
		<> help "get configuration"
		<> metavar paramName
		)
	unsetconfig = UnsetConfig <$> strOption
		( long "unset"
		<> help "unset configuration"
		<> metavar paramName
		)

seek :: Action -> CommandSeek
seek (SetConfig name val) = commandAction $
	startingUsualMessages name (ActionItemOther (Just val)) $ do
		setGlobalConfig name val
		setConfig (ConfigKey name) val
		next $ return True
seek (UnsetConfig name) = commandAction $
	startingUsualMessages name (ActionItemOther (Just "unset")) $do
		unsetGlobalConfig name
		unsetConfig (ConfigKey name)
		next $ return True
seek (GetConfig name) = commandAction $
	startingCustomOutput (ActionItemOther Nothing) $ do
		getGlobalConfig name >>= \case
			Nothing -> return ()
			Just v -> liftIO $ putStrLn v
		next $ return True

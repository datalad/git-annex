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
import Git.Types (ConfigKey(..), fromConfigValue)

import qualified Data.ByteString as S

cmd :: Command
cmd = noMessages $ command "config" SectionSetup
	"configuration stored in git-annex branch"
	paramNothing (seek <$$> optParser)

data Action
	= SetConfig ConfigKey ConfigValue
	| GetConfig ConfigKey
	| UnsetConfig ConfigKey

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
seek (SetConfig ck@(ConfigKey name) val) = commandAction $
	startingUsualMessages (decodeBS' name) (ActionItemOther (Just (fromConfigValue val))) $ do
		setGlobalConfig ck val
		setConfig ck (fromConfigValue val)
		next $ return True
seek (UnsetConfig ck@(ConfigKey name)) = commandAction $
	startingUsualMessages (decodeBS' name) (ActionItemOther (Just "unset")) $do
		unsetGlobalConfig ck
		unsetConfig ck
		next $ return True
seek (GetConfig ck) = commandAction $
	startingCustomOutput (ActionItemOther Nothing) $ do
		getGlobalConfig ck >>= \case
			Nothing -> return ()
			Just (ConfigValue v) -> liftIO $ S.putStrLn v
		next $ return True

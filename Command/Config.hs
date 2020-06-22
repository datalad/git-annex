{- git-annex command
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Config where

import Command
import Logs.Config
import Config
import Types.GitConfig (globalConfigs)
import Git.Types (fromConfigValue)

import qualified Data.ByteString.Char8 as S8

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
seek (SetConfig ck@(ConfigKey name) val) = checkIsGlobalConfig ck $ commandAction $
	startingUsualMessages (decodeBS' name) (ActionItemOther (Just (fromConfigValue val))) $ do
		setGlobalConfig ck val
		when (needLocalUpdate ck) $
			setConfig ck (fromConfigValue val)
		next $ return True
seek (UnsetConfig ck@(ConfigKey name)) = checkIsGlobalConfig ck $ commandAction $
	startingUsualMessages (decodeBS' name) (ActionItemOther (Just "unset")) $do
		unsetGlobalConfig ck
		when (needLocalUpdate ck) $
			unsetConfig ck
		next $ return True
seek (GetConfig ck) = checkIsGlobalConfig ck $ commandAction $
	startingCustomOutput (ActionItemOther Nothing) $ do
		getGlobalConfig ck >>= \case
			Just (ConfigValue v) -> liftIO $ S8.putStrLn v
			Just NoConfigValue -> return ()
			Nothing -> return ()
		next $ return True

checkIsGlobalConfig :: ConfigKey -> Annex a -> Annex a
checkIsGlobalConfig ck@(ConfigKey name) a
	| elem ck globalConfigs = a
	| otherwise = giveup $ decodeBS name ++ " is not a configuration setting that can be stored in the git-annex branch"

needLocalUpdate :: ConfigKey -> Bool
needLocalUpdate (ConfigKey "annex.securehashesonly") = True
needLocalUpdate _ = False

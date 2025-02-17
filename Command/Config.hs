{- git-annex command
 -
 - Copyright 2017-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Config where

import Command
import Logs.Config
import Config
import Types.GitConfig (globalConfigs)
import Git.Types (fromConfigValue, fromConfigKey)
import qualified Git.Command
import Utility.SafeOutput
import Annex.CheckAttr
import Types.NumCopies
import Logs.NumCopies

import qualified Data.ByteString.Char8 as S8

cmd :: Command
cmd = noMessages $ command "config" SectionSetup
	"configuration stored in git-annex branch"
	paramNothing (seek <$$> optParser)

data Action
	= SetConfig ConfigKey ConfigValue
	| GetConfig ConfigKey
	| UnsetConfig ConfigKey
	| ShowOrigin ConfigKey (Maybe FilePath)

type Name = String
type Value = String

optParser :: CmdParamsDesc -> Parser Action
optParser _ = setconfig <|> getconfig <|> unsetconfig <|> showorigin
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
	showorigin = ShowOrigin
		<$> strOption
			( long "show-origin"
			<> help "explain where a value is configured"
			<> metavar paramName
			)
		<*> optional (strOption
			( long "for-file"
			<> help "filename to check for in gitattributes"
			<> metavar paramFile
			))

seek :: Action -> CommandSeek
seek (SetConfig ck@(ConfigKey name) val) = checkIsGlobalConfig ck $ \setter _unsetter _getter ->
	commandAction $ startingUsualMessages (decodeBS name) ai si $ do
		setter val
		when (needLocalUpdate ck) $
			setConfig ck (fromConfigValue val)
		next $ return True
  where
	ai = ActionItemOther (Just (UnquotedString (fromConfigValue val)))
	si = SeekInput [decodeBS name]
seek (UnsetConfig ck@(ConfigKey name)) = checkIsGlobalConfig ck $ \_setter unsetter _getter ->
	commandAction $ startingUsualMessages (decodeBS name) ai si $ do
		unsetter
		when (needLocalUpdate ck) $
			unsetConfig ck
		next $ return True
  where
	ai = ActionItemOther (Just "unset")
	si = SeekInput [decodeBS name]
seek (GetConfig ck) = checkIsGlobalConfig ck $ \_setter _unsetter getter ->
	commandAction $	startingCustomOutput ai $ do
		getter >>= \case
			Just (ConfigValue v) -> liftIO $ S8.putStrLn $ safeOutput v
			Just NoConfigValue -> return ()
			Nothing -> return ()
		next $ return True
  where
	ai = ActionItemOther Nothing
seek (ShowOrigin ck@(ConfigKey name) forfile) = commandAction $
	startingCustomOutput ai $ next $ checknotconfigured $
		case checkIsGlobalConfig' ck of
			Just (_setter, _unsetter, getter) ->
				ifM gitconfigorigin
					( return True
					, checkattrs (checkconfigbranch getter)
					)
			Nothing -> ifM gitconfigorigin
				( return True
				, checkattrs checkgitconfigunderride
				)
  where
	ai = ActionItemOther Nothing

	gitconfigorigin
		| name `elem` gitconfigdoesnotoverride = return False
		| otherwise = gitconfigorigin'
	gitconfigorigin' = inRepo $ Git.Command.runBool
			[ Param "config"
			, Param "--show-origin"
			, Param (decodeBS name)
			]
	
	-- git configs for these do not override values from git attributes
	-- or the branch
	gitconfigdoesnotoverride =
		[ "annex.numcopies"
		, "annex.mincopies"
		]

	-- the git config for annex.numcopies is a special case; it's only
	-- used if not configured anywhere else
	checkgitconfigunderride
		| name == "annex.numcopies" = gitconfigorigin'
		| otherwise = return False

	-- Display similar to git config --show-origin
	showval loc v = liftIO $ do
		putStrLn $ loc ++ "\t" ++ v
		return True
	
	configbranch v
		| needLocalUpdate ck = checkgitconfigunderride
		| otherwise = showval "branch:git-annex" (decodeBS v)
	
	checkconfigbranch getter = getter >>= \case
		Just (ConfigValue v) -> configbranch v
		_ -> checkgitconfigunderride
	
	checkattrs cont
		| decodeBS name `elem` annexAttrs =
			case forfile of
				Just file -> do
					v <- checkAttr (decodeBS name) (toOsPath file)
					if null v
						then cont
						else showval "gitattributes" v		
				Nothing -> do
					warnforfile
					cont
		| otherwise = cont
	
	warnforfile = warning $ UnquotedString $ configKeyMessage ck $ unwords
		[ "may be configured in gitattributes."
		, "Pass --for-file= with a filename to check"
		]
	
	checknotconfigured a = do
		ok <- a
		unless ok $
			warning $ UnquotedString $ configKeyMessage ck
				"is not configured"
		return ok

type Setter = ConfigValue -> Annex ()
type Unsetter = Annex ()
type Getter = Annex (Maybe ConfigValue)

checkIsGlobalConfig :: ConfigKey -> (Setter -> Unsetter -> Getter -> Annex a) -> Annex a
checkIsGlobalConfig ck a = case checkIsGlobalConfig' ck of
	Just (setter, unsetter, getter) -> a setter unsetter getter
	Nothing -> giveup $ configKeyMessage ck "is not a configuration setting that can be stored in the git-annex branch"

checkIsGlobalConfig' :: ConfigKey -> Maybe (Setter, Unsetter, Getter)
checkIsGlobalConfig' ck
	| elem ck globalConfigs = Just
		( setGlobalConfig ck
		, unsetGlobalConfig ck
		, getGlobalConfig ck
		)
	-- These came before this command, but are also global configs,
	-- so support them here as well.
	| ck == ConfigKey "annex.numcopies" = Just
		( mksetter (setGlobalNumCopies . configuredNumCopies)
		, error "unsetting annex.numcopies is not supported"
		, mkgetter fromNumCopies getGlobalNumCopies
		)
	| ck == ConfigKey "annex.mincopies" = Just
		( mksetter (setGlobalMinCopies . configuredMinCopies)
		, error "unsetting annex.mincopies is not supported"
		, mkgetter fromMinCopies getGlobalMinCopies
		)
	| otherwise = Nothing
  where
	mksetter f = 
		maybe (error ("invalid value for " ++ fromConfigKey ck)) f 
			. readish . decodeBS . fromConfigValue
	mkgetter f g = fmap (ConfigValue . encodeBS . show . f) <$> g

configKeyMessage :: ConfigKey -> String -> String
configKeyMessage (ConfigKey name) msg = decodeBS name ++ " " ++ msg

needLocalUpdate :: ConfigKey -> Bool
needLocalUpdate (ConfigKey "annex.securehashesonly") = True
needLocalUpdate _ = False

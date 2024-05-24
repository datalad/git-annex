{- git-annex command
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.InitRemote where

import Command
import Annex.SpecialRemote
import qualified Remote
import qualified Types.Remote as R
import Types.RemoteConfig
import Annex.UUID
import Logs.UUID
import Logs.Remote
import Types.GitConfig
import Types.ProposedAccepted
import Config
import Git.Config
import Git.Types

import qualified Data.Map as M
import qualified Data.Text as T

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "initremote" SectionSetup
		"creates a special (non-git) remote"
		(paramPair paramName $ paramOptional $ paramRepeating paramParamValue)
		(seek <$$> optParser)

data InitRemoteOptions = InitRemoteOptions
	{ cmdparams :: CmdParams
	, sameas :: Maybe (DeferredParse UUID)
	, withUrl :: Bool
	, whatElse :: Bool
	, privateRemote :: Bool
	}

optParser :: CmdParamsDesc -> Parser InitRemoteOptions
optParser desc = InitRemoteOptions
	<$> cmdParams desc
	<*> optional parseSameasOption
	<*> switch
		( long "with-url"
		<> short 'u'
		<> help "configure remote with an annex:: url"
		)
	<*> switch
		( long "whatelse"
		<> short 'w'
		<> help "describe other configuration parameters for a special remote"
		)
	<*> switch
		( long "private"
		<> help "keep special remote information out of git-annex branch"
		)

parseSameasOption :: Parser (DeferredParse UUID)
parseSameasOption = parseUUIDOption <$> strOption
	( long "sameas"
	<> metavar (paramRemote `paramOr` paramDesc `paramOr` paramUUID)
	<> help "new remote that accesses the same data"
	<> completeRemotes
	)

seek :: InitRemoteOptions -> CommandSeek
seek o = withWords (commandAction . (start o)) (cmdparams o)

start :: InitRemoteOptions -> [String] -> CommandStart
start _ [] = giveup "Specify a name for the remote."
start o (name:ws) = do
	if whatElse o
		then ifM jsonOutputEnabled
			( starting "initremote" ai si $ prep $ \c t ->
				describeOtherParamsFor c t
			, startingCustomOutput (ActionItemOther Nothing) $ prep $ \c t ->
				describeOtherParamsFor c t
			)
		else starting "initremote" ai si $ prep $ \c t ->
			perform t name c o
  where
	prep a = do
		whenM (not . null <$> findExisting name) $
			giveup $ "There is already a special remote named \"" ++ name ++
				"\". (Use enableremote to enable an existing special remote.)"
		whenM (isJust <$> Remote.byNameOnly name) $
			giveup $ "There is already a remote named \"" ++ name ++ "\""
		sameasuuid <- maybe
			(pure Nothing)
			(Just . Sameas <$$> getParsed)
			(sameas o) 
		c <- newConfig name sameasuuid
			(Logs.Remote.keyValToConfig Proposed ws)
			<$> remoteConfigMap
		t <- either giveup return (findType c)
		a c t
	
	si = SeekInput (name:ws)
	ai = ActionItemOther (Just (UnquotedString name))

perform :: RemoteType -> String -> R.RemoteConfig -> InitRemoteOptions -> CommandPerform
perform t name c o = do
	when (privateRemote o) $
		setConfig (remoteAnnexConfig c "private") (boolConfig True)
	dummycfg <- liftIO dummyRemoteGitConfig
	let c' = M.delete uuidField c
	(c'', u) <- R.setup t R.Init (sameasu <|> uuidfromuser) Nothing c' dummycfg
	next $ cleanup t u name c'' o
  where
	uuidfromuser = case fromProposedAccepted <$> M.lookup uuidField c of
		Just s
			| isUUID s -> Just (toUUID s)
			| otherwise -> giveup "invalid uuid"
		Nothing -> Nothing
	sameasu = toUUID . fromProposedAccepted <$> M.lookup sameasUUIDField c

uuidField :: R.RemoteConfigField
uuidField = Accepted "uuid"

cleanup :: RemoteType -> UUID -> String -> R.RemoteConfig -> InitRemoteOptions -> CommandCleanup
cleanup t u name c o = do
	case sameas o of
		Nothing -> do
			describeUUID u (toUUIDDesc name)
			Logs.Remote.configSet u c
		Just _ -> do
			cu <- liftIO genUUID
			setConfig (remoteAnnexConfig c "config-uuid") (fromUUID cu)
			Logs.Remote.configSet cu c
	when (withUrl o) $
		setAnnexUrl c
	unless (Remote.gitSyncableRemoteType t || withUrl o) $
		setConfig (remoteConfig c "skipFetchAll") (boolConfig True)
	return True

setAnnexUrl :: R.RemoteConfig -> Annex ()
setAnnexUrl c =
	getConfigMaybe (remoteConfig c "url") >>= \case
		Just (ConfigValue _) -> noop
		_ -> do
			setConfig (remoteConfig c "url") "annex::"
			setConfig (remoteConfig c "fetch") $
				"+refs/heads/*:refs/remotes/" ++
				getRemoteName c ++ "/*"

describeOtherParamsFor :: RemoteConfig -> RemoteType -> CommandPerform
describeOtherParamsFor c t = do
	cp <- R.configParser t c
	let l = map mk (filter notinconfig $ remoteConfigFieldParsers cp)
		++ map mk' (maybe [] snd (remoteConfigRestPassthrough cp))
	ifM jsonOutputEnabled
		( maybeAddJSONField "whatelse" $ M.fromList $ mkjson l
		, liftIO $ forM_ l $ \(p, fd, vd) -> case fd of
			HiddenField -> return ()
			DeprecatedField -> return ()
			FieldDesc d -> do
				putStrLn p
				putStrLn ("\t" ++ d)
				case vd of
					Nothing -> return ()
					Just (ValueDesc d') ->
						putStrLn $ "\t(" ++ d' ++ ")"
		
		)
	next $ return True
  where
	mkjson = mapMaybe $ \(p, fd, vd) ->
		case fd of
			HiddenField -> Nothing
			DeprecatedField -> Nothing
			FieldDesc d -> Just 
				( T.pack p
				, M.fromList
					[ ("description" :: T.Text, d)
					, ("valuedescription", case vd of
						Nothing -> ""
						Just (ValueDesc d') -> d')
					]
				)

	notinconfig fp = not (M.member (parserForField fp) c)

	mk fp = ( fromProposedAccepted (parserForField fp)
		, fieldDesc fp
		, valueDesc fp
		)
	mk' (k, v) = (k, v, Nothing)

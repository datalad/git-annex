{- git-annex command
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.InitRemote where

import qualified Data.Map as M

import Command
import Annex.SpecialRemote
import qualified Remote
import qualified Logs.Remote
import qualified Types.Remote as R
import Types.RemoteConfig
import Annex.UUID
import Logs.UUID
import Logs.Remote
import Types.GitConfig
import Types.ProposedAccepted
import Config

cmd :: Command
cmd = command "initremote" SectionSetup
	"creates a special (non-git) remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramParamValue)
	(seek <$$> optParser)

data InitRemoteOptions = InitRemoteOptions
	{ cmdparams :: CmdParams
	, sameas :: Maybe (DeferredParse UUID)
	, whatElse :: Bool
	}

optParser :: CmdParamsDesc -> Parser InitRemoteOptions
optParser desc = InitRemoteOptions
	<$> cmdParams desc
	<*> optional parseSameasOption
	<*> switch
		( long "whatelse"
		<> short 'w'
		<> help "describe other configuration parameters for a special remote"
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
start o (name:ws) = ifM (isJust <$> findExisting name)
	( giveup $ "There is already a special remote named \"" ++ name ++
		"\". (Use enableremote to enable an existing special remote.)"
	, do
		ifM (isJust <$> Remote.byNameOnly name)
			( giveup $ "There is already a remote named \"" ++ name ++ "\""
			, do
				sameasuuid <- maybe
					(pure Nothing)
					(Just . Sameas <$$> getParsed)
					(sameas o) 
				c <- newConfig name sameasuuid
					(Logs.Remote.keyValToConfig Proposed ws)
					<$> readRemoteLog
				t <- either giveup return (findType c)
				if whatElse o
					then startingCustomOutput (ActionItemOther Nothing) $
						describeOtherParamsFor c t
					else starting "initremote" (ActionItemOther (Just name)) $
						perform t name c o
			)
	)

perform :: RemoteType -> String -> R.RemoteConfig -> InitRemoteOptions -> CommandPerform
perform t name c o = do
	dummycfg <- liftIO dummyRemoteGitConfig
	let c' = M.delete uuidField c
	(c'', u) <- R.setup t R.Init (sameasu <|> uuidfromuser) Nothing c' dummycfg
	next $ cleanup u name c'' o
  where
	uuidfromuser = case fromProposedAccepted <$> M.lookup uuidField c of
		Just s
			| isUUID s -> Just (toUUID s)
			| otherwise -> giveup "invalid uuid"
		Nothing -> Nothing
	sameasu = toUUID . fromProposedAccepted <$> M.lookup sameasUUIDField c

uuidField :: R.RemoteConfigField
uuidField = Accepted "uuid"

cleanup :: UUID -> String -> R.RemoteConfig -> InitRemoteOptions -> CommandCleanup
cleanup u name c o = do
	case sameas o of
		Nothing -> do
			describeUUID u (toUUIDDesc name)
			Logs.Remote.configSet u c
		Just _ -> do
			cu <- liftIO genUUID
			setConfig (remoteAnnexConfig c "config-uuid") (fromUUID cu)
			Logs.Remote.configSet cu c
	return True

describeOtherParamsFor :: RemoteConfig -> RemoteType -> CommandPerform
describeOtherParamsFor c t = do
	cp <- R.configParser t c
	let l = map mk (filter notinconfig $ remoteConfigFieldParsers cp)
		++ map mk' (maybe [] snd (remoteConfigRestPassthrough cp))
	liftIO $ forM_ l $ \(p, fd, vd) -> case fd of
		HiddenField -> return ()
		FieldDesc d -> do
			putStrLn p
			putStrLn ("\t" ++ d)
			case vd of
				Nothing -> return ()
				Just (ValueDesc d') ->
					putStrLn $ "\t(" ++ d' ++ ")"
	next $ return True
  where
	notinconfig fp = not (M.member (parserForField fp) c)
	mk fp = ( fromProposedAccepted (parserForField fp)
		, fieldDesc fp
		, valueDesc fp
		)
	mk' (k, v) = (k, v, Nothing)

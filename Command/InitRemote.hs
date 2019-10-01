{- git-annex command
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.InitRemote where

import qualified Data.Map as M

import Command
import Annex.SpecialRemote
import qualified Remote
import qualified Logs.Remote
import qualified Types.Remote as R
import Logs.UUID
import Types.GitConfig

cmd :: Command
cmd = command "initremote" SectionSetup
	"creates a special (non-git) remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramKeyValue)
	(seek <$$> optParser)

data InitRemoteOptions = InitRemoteOptions
	{ cmdparams :: CmdParams
	, sameas :: Maybe (DeferredParse UUID)
	}

optParser :: CmdParamsDesc -> Parser InitRemoteOptions
optParser desc = InitRemoteOptions
	<$> cmdParams desc
	<*> optional parseSameasOption

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
				let c = newConfig name
				t <- either giveup return (findType config)
				starting "initremote" (ActionItemOther (Just name)) $
					perform t name $ M.union config c
			)
	)
  where
	config = Logs.Remote.keyValToConfig ws

perform :: RemoteType -> String -> R.RemoteConfig -> CommandPerform
perform t name c = do
	dummycfg <- liftIO dummyRemoteGitConfig
	(c', u) <- R.setup t R.Init cu Nothing c dummycfg
	next $ cleanup u name c'
  where
	cu = case M.lookup "uuid" c of
		Just s
			| isUUID s -> Just (toUUID s)
			| otherwise -> giveup "invalid uuid"
		Nothing -> Nothing

cleanup :: UUID -> String -> R.RemoteConfig -> CommandCleanup
cleanup u name c = do
	describeUUID u (toUUIDDesc name)
	Logs.Remote.configSet u c
	return True

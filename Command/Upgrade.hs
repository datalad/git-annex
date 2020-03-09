{- git-annex command
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Upgrade where

import Command
import Upgrade
import Annex.Version
import Annex.Init

cmd :: Command
cmd = dontCheck repoExists $
	-- ^ because an old version may not seem to exist
	-- and also, this avoids automatic silent upgrades before
	-- this command can start up.
	noDaemonRunning $
	-- ^ avoid upgrading repo out from under daemon
	command "upgrade" SectionMaintenance "upgrade repository"
		paramNothing (seek <$$> optParser)

data UpgradeOptions = UpgradeOptions
	{ autoOnly :: Bool
	}

optParser :: CmdParamsDesc -> Parser UpgradeOptions
optParser _ = UpgradeOptions
	<$> switch
		( long "autoonly"
		<> help "only do automatic upgrades"
		)

seek :: UpgradeOptions -> CommandSeek
seek o = commandAction (start o)

start :: UpgradeOptions -> CommandStart
start (UpgradeOptions { autoOnly = True }) = do
	starting "upgrade" (ActionItemOther Nothing) $ do
	getVersion >>= maybe noop checkUpgrade
	next $ return True
start _ = starting "upgrade" (ActionItemOther Nothing) $ do
	whenM (isNothing <$> getVersion) $ do
		initialize Nothing Nothing
	r <- upgrade False latestVersion
	next $ return r

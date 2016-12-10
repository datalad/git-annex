{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.EnableTor where

import Command
import P2P.Address
import Utility.Tor
import Annex.UUID

-- This runs as root, so avoid making any commits or initializing
-- git-annex, or doing other things that create root-owned files.
cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	command "enable-tor" SectionSetup "enable tor hidden service"
		"uid" (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ps = case readish =<< headMaybe ps of
	Nothing -> giveup "Bad params"
	Just userid -> do
		uuid <- getUUID
		when (uuid == NoUUID) $
			giveup "This can only be run in a git-annex repository."
		(onionaddr, onionport) <- liftIO $
			addHiddenService userid (fromUUID uuid)
		storeP2PAddress $ TorAnnex onionaddr onionport
		stop

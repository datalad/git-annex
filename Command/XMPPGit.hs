{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.XMPPGit where

import Common.Annex
import Command
import Assistant.XMPP.Git

def :: [Command]
def = [noCommit $ noRepo xmppGitRelay $ dontCheck repoExists $
	command "xmppgit" paramNothing seek "git to XMPP relay (internal use)"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start _ = do
	liftIO xmppGitRelay
	stop

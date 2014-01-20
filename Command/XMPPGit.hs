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
def = [noCommit $ noRepo startNoRepo $ dontCheck repoExists $
	command "xmppgit" paramNothing seek
		SectionPlumbing "git to XMPP relay"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start _ = do
	liftIO gitRemoteHelper
	liftIO xmppGitRelay
	stop

startNoRepo :: CmdParams -> IO ()
startNoRepo _ = xmppGitRelay

{- A basic implementation of the git-remote-helpers protocol. -}
gitRemoteHelper :: IO ()
gitRemoteHelper = do
	expect "capabilities"
	respond ["connect"]
	expect "connect git-receive-pack"
	respond []
  where
	expect s = do
		cmd <- getLine
		unless (cmd == s) $
			error $ "git-remote-helpers protocol error: expected: " ++ s ++ ", but got: " ++ cmd
	respond l = do
		mapM_ putStrLn l
		putStrLn ""
		hFlush stdout

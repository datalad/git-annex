{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.XMPPGit where

import Command
import Assistant.XMPP.Git

cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	noRepo (parseparams startNoRepo) $ 
		command "xmppgit" SectionPlumbing "git to XMPP relay"
			paramNothing (parseparams seek)
  where
	parseparams = withParams

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: CmdParams -> CommandStart
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
		gitcmd <- getLine
		unless (gitcmd == s) $
			error $ "git-remote-helpers protocol error: expected: " ++ s ++ ", but got: " ++ gitcmd
	respond l = do
		mapM_ putStrLn l
		putStrLn ""
		hFlush stdout

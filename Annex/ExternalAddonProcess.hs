{- External addon processes for special remotes and backends.
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.ExternalAddonProcess where

import qualified Annex
import Annex.Common
import Git.Env
import Utility.Shell
import Messages.Progress

import Control.Concurrent.Async
import System.Log.Logger (debugM)

data ExternalAddonProcess = ExternalAddonProcess
	{ externalSend :: Handle
	, externalReceive :: Handle
	-- Shut down the process. With True, it's forced to stop
	-- immediately.
	, externalShutdown :: Bool -> IO ()
	, externalPid :: ExternalAddonPID
	, externalProgram :: String
	}

type ExternalAddonPID = Int

data ExternalAddonStartError
	= ProgramNotInstalled String
	| ProgramFailure String

startExternalAddonProcess :: String -> ExternalAddonPID -> Annex (Either ExternalAddonStartError ExternalAddonProcess)
startExternalAddonProcess basecmd pid = do
	errrelayer <- mkStderrRelayer
	g <- Annex.gitRepo
	cmdpath <- liftIO $ searchPath basecmd
	liftIO $ start errrelayer g cmdpath
  where
	start errrelayer g cmdpath = do
		(cmd, ps) <- maybe (pure (basecmd, [])) findShellCommand cmdpath
		let basep = (proc cmd (toCommand ps))
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = CreatePipe
			}
		p <- propgit g basep
		tryNonAsync (createProcess p) >>= \case
			Right v -> (Right <$> started cmd errrelayer v)
				`catchNonAsync` const (runerr cmdpath)
			Left _ -> runerr cmdpath
	
	started cmd errrelayer pall@(Just hin, Just hout, Just herr, ph) = do
		stderrelay <- async $ errrelayer herr
		let shutdown forcestop = do
			-- Close the process's stdin, to let it know there
			-- are no more requests, so it will exit.
			hClose hout
			-- Close the procces's stdout as we're not going to
			-- process any more output from it.
			hClose hin
			if forcestop
				then cleanupProcess pall
				else void (waitForProcess ph)
					`onException` cleanupProcess pall
			-- This thread will exit after consuming any
			-- remaining stderr from the process.
			() <- wait stderrelay
			hClose herr
		return $ ExternalAddonProcess
			{ externalSend = hin
			, externalReceive = hout
			, externalPid = pid
			, externalShutdown = shutdown
			, externalProgram = cmd
			}
	started _ _ _ = giveup "internal"

	propgit g p = do
		environ <- propGitEnv g
		return $ p { env = Just environ }

	runerr (Just cmd) =
		return $ Left $ ProgramFailure $
			"Cannot run " ++ cmd ++ " -- Make sure it's executable and that its dependencies are installed."
	runerr Nothing = do
		path <- intercalate ":" <$> getSearchPath
		return $ Left $ ProgramNotInstalled $
			"Cannot run " ++ basecmd ++ " -- It is not installed in PATH (" ++ path ++ ")"

protocolDebug :: ExternalAddonProcess -> Bool -> String -> IO ()
protocolDebug external sendto line = debugM "external" $ unwords
	[ externalProgram external ++ 
		"[" ++ show (externalPid external) ++ "]"
	, if sendto then "<--" else "-->"
	, line
	]

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

import Control.Concurrent.STM
import Control.Concurrent.Async

data ExternalAddonProcess = ExternalAddonProcess
	{ externalSend :: Handle
	, externalReceive :: Handle
	-- Shut down the process. With True, it's forced to stop
	-- immediately.
	, externalShutdown :: Bool -> IO ()
	, externalPid :: ExternalAddonPID
	}

type ExternalAddonPID = Int

data ExternalAddonStartError
	= ProgramNotInstalled String
	| ProgramFailure String

startExternalAddonProcess :: String -> TVar ExternalAddonPID-> Annex (Either ExternalAddonStartError ExternalAddonProcess)
startExternalAddonProcess basecmd pidvar = do
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
			Right v -> (Right <$> started errrelayer v)
				`catchNonAsync` const (runerr cmdpath)
			Left _ -> runerr cmdpath
	
	started errrelayer pall@(Just hin, Just hout, Just herr, ph) = do
		stderrelay <- async $ errrelayer herr
		pid <- atomically $ do
			n <- succ <$> readTVar pidvar
			writeTVar pidvar n
			return n
		let shutdown forcestop = do
			cancel stderrelay
			if forcestop
				then cleanupProcess pall
				else flip onException (cleanupProcess pall) $ do
					hClose herr
					hClose hin
					hClose hout
					void $ waitForProcess ph
		return $ ExternalAddonProcess
			{ externalSend = hin
			, externalReceive = hout
			, externalPid = pid
			, externalShutdown = shutdown
			}
	started _ _ = giveup "internal"

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

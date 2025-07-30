{- External addon processes for special remotes and backends.
 -
 - Copyright 2013-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.ExternalAddonProcess where

import qualified Annex
import Annex.Common
import Git.Env
import Utility.Shell
import Messages.Progress

import Control.Concurrent.Async

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

externalAddonStartErr :: Maybe OsPath -> String -> IO ExternalAddonStartError
externalAddonStartErr (Just cmd) _ =
		return $ ProgramFailure $
			"Cannot run " ++ fromOsPath cmd ++ " -- Make sure it's executable and that its dependencies are installed."
externalAddonStartErr Nothing basecmd = do
		path <- intercalate ":" . map fromOsPath <$> getSearchPath
		return $ ProgramNotInstalled $
			"Cannot run " ++ basecmd ++ " -- It is not installed in PATH (" ++ path ++ ")"

startExternalAddonProcess
	:: (CreateProcess -> CreateProcess)
	-> String
	-> [CommandParam]
	-> IO (Either ExternalAddonStartError (String, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)))
startExternalAddonProcess f basecmd ps = do
	cmdpath <- searchPath basecmd
	startExternalAddonProcess' cmdpath f basecmd ps

startExternalAddonProcess'
	:: Maybe OsPath
	-> (CreateProcess -> CreateProcess)
	-> String
	-> [CommandParam]
	-> IO (Either ExternalAddonStartError (String, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)))
startExternalAddonProcess' cmdpath mkproc basecmd ps = do
	(cmd, cmdps) <- maybe (pure (basecmd, [])) findShellCommand cmdpath
	let p = mkproc (proc cmd (toCommand (cmdps ++ ps)))
	tryNonAsync (createProcess p) >>= \case
		Right v -> return (Right (cmd, v))
		Left _ -> Left <$> externalAddonStartErr cmdpath basecmd

-- | Starts an external addon process that speaks a protocol over stdio.
startExternalAddonProcessProtocol :: String -> [CommandParam] -> ExternalAddonPID -> Annex (Either ExternalAddonStartError ExternalAddonProcess)
startExternalAddonProcessProtocol basecmd ps pid = do
	errrelayer <- mkStderrRelayer
	g <- Annex.gitRepo
	cmdpath <- liftIO $ searchPath basecmd
	liftIO $ start errrelayer g cmdpath
  where
	start errrelayer g cmdpath = do
		environ <- propGitEnv g
		let mkproc = \p -> p
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = CreatePipe
			, env = Just environ
			}
		startExternalAddonProcess' cmdpath mkproc basecmd ps >>= \case
			Right (cmd, v) -> (Right <$> started cmd errrelayer v)
				`catchNonAsync` const (Left <$> externalAddonStartErr cmdpath basecmd)
			Left err -> return (Left err)
	
	started cmd errrelayer pall@(Just hin, Just hout, Just herr, ph) = do
		stderrelay <- async $ errrelayer ph herr
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

protocolDebug :: ExternalAddonProcess -> Bool -> String -> IO ()
protocolDebug external sendto line = debug "Annex.ExternalAddonProcess" $ unwords
	[ externalProgram external ++ 
		"[" ++ show (externalPid external) ++ "]"
	, if sendto then "<--" else "-->"
	, line
	]

{- git-annex assistant
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Assistant where

import Common.Annex
import Command
import qualified Command.Watch
import Annex.Init
import Annex.Path
import Config.Files
import qualified Build.SysConfig
import Utility.HumanTime
import Assistant.Install

cmd :: Command
cmd = dontCheck repoExists $ notBareRepo $
	noRepo (startNoRepo <$$> optParser) $
		command "assistant" SectionCommon
			"automatically sync changes"
			paramNothing (seek <$$> optParser)

data AssistantOptions = AssistantOptions
	{ daemonOptions :: DaemonOptions
	, autoStartOption :: Bool
	, startDelayOption :: Maybe Duration
	, autoStopOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser AssistantOptions
optParser _ = AssistantOptions
	<$> parseDaemonOptions
	<*> switch
		( long "autostart"
		<> help "start in known repositories"
		)
	<*> optional (option (str >>= parseDuration)
		( long "startdelay" <> metavar paramNumber
		<> help "delay before running startup scan"
		))
	<*> switch
		( long "autostop"
		<> help "stop in known repositories"
		)

seek :: AssistantOptions -> CommandSeek
seek = commandAction . start

start :: AssistantOptions -> CommandStart
start o
	| autoStartOption o = do
		liftIO $ autoStart o
		stop
	| autoStopOption o = do
		liftIO autoStop
		stop
	| otherwise = do
		liftIO ensureInstalled
		ensureInitialized
		Command.Watch.start True (daemonOptions o) (startDelayOption o)

startNoRepo :: AssistantOptions -> IO ()
startNoRepo o
	| autoStartOption o = autoStart o
	| autoStopOption o = autoStop
	| otherwise = error "Not in a git repository."

autoStart :: AssistantOptions -> IO ()
autoStart o = do
	dirs <- liftIO readAutoStartFile
	when (null dirs) $ do
		f <- autoStartFile
		error $ "Nothing listed in " ++ f
	program <- programPath
	haveionice <- pure Build.SysConfig.ionice <&&> inPath "ionice"
	forM_ dirs $ \d -> do
		putStrLn $ "git-annex autostart in " ++ d
		ifM (catchBoolIO $ go haveionice program d)
			( putStrLn "ok"
			, putStrLn "failed"
			)
  where
	go haveionice program dir = do
		setCurrentDirectory dir
		-- First stop any old daemon running in this directory, which
		-- might be a leftover from an old login session. Such a
		-- leftover might be left in an environment where it is
		-- unavble to use the ssh agent or other login session
		-- resources.
		void $ boolSystem program [Param "assistant", Param "--stop"]
		if haveionice
			then boolSystem "ionice" (Param "-c3" : Param program : baseparams)
			else boolSystem program baseparams
	  where
		baseparams =
			[ Param "assistant"
			, Param $ "--startdelay=" ++ fromDuration (fromMaybe (Duration 5) (startDelayOption o))
			]

autoStop :: IO ()
autoStop = do
	dirs <- liftIO readAutoStartFile
	program <- programPath
	forM_ dirs $ \d -> do
		putStrLn $ "git-annex autostop in " ++ d
		setCurrentDirectory d
		ifM (boolSystem program [Param "assistant", Param "--stop"])
			( putStrLn "ok"
			, putStrLn "failed"
			)

{- git-annex assistant
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
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

import System.Environment

cmd :: [Command]
cmd = [noRepo checkNoRepoOpts $ dontCheck repoExists $ withOptions options $
	notBareRepo $ command "assistant" paramNothing seek SectionCommon
		"automatically sync changes"]

options :: [Option]
options =
	[ Command.Watch.foregroundOption
	, Command.Watch.stopOption
	, autoStartOption
	, startDelayOption
	, autoStopOption
	]

autoStartOption :: Option
autoStartOption = flagOption [] "autostart" "start in known repositories"

autoStopOption :: Option
autoStopOption = flagOption [] "autostop" "stop in known repositories"

startDelayOption :: Option
startDelayOption = fieldOption [] "startdelay" paramNumber "delay before running startup scan"

seek :: CommandSeek
seek ps = do
	stopdaemon <- getOptionFlag Command.Watch.stopOption
	foreground <- getOptionFlag Command.Watch.foregroundOption
	autostart <- getOptionFlag autoStartOption
	autostop <- getOptionFlag autoStopOption
	startdelay <- getOptionField startDelayOption (pure . maybe Nothing parseDuration)
	withNothing (start foreground stopdaemon autostart autostop startdelay) ps

start :: Bool -> Bool -> Bool -> Bool -> Maybe Duration -> CommandStart
start foreground stopdaemon autostart autostop startdelay
	| autostart = do
		liftIO $ autoStart startdelay
		stop
	| autostop = do
		liftIO autoStop
		stop
	| otherwise = do
		liftIO ensureInstalled
		ensureInitialized
		Command.Watch.start True foreground stopdaemon startdelay

{- Run outside a git repository; support autostart and autostop mode. -}
checkNoRepoOpts :: CmdParams -> IO ()
checkNoRepoOpts _ = ifM (elem "--autostart" <$> getArgs)
	( autoStart Nothing
	, ifM (elem "--autostop" <$> getArgs)
		( autoStop
		, error "Not in a git repository."
		)
	) 

autoStart :: Maybe Duration -> IO ()
autoStart startdelay = do
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
		if haveionice
			then boolSystem "ionice" (Param "-c3" : Param program : baseparams)
			else boolSystem program baseparams
	  where
		baseparams =
			[ Param "assistant"
			, Param $ "--startdelay=" ++ fromDuration (fromMaybe (Duration 5) startdelay)
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

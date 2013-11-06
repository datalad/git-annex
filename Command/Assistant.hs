{- git-annex assistant
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Assistant where

import Common.Annex
import Command
import qualified Option
import qualified Command.Watch
import Init
import Config.Files
import qualified Build.SysConfig
import Utility.HumanTime

import System.Environment

def :: [Command]
def = [noRepo checkAutoStart $ dontCheck repoExists $ withOptions options $
	command "assistant" paramNothing seek SectionCommon
		"automatically handle changes"]

options :: [Option]
options =
	[ Command.Watch.foregroundOption
	, Command.Watch.stopOption
	, autoStartOption
	, startDelayOption
	]

autoStartOption :: Option
autoStartOption = Option.flag [] "autostart" "start in known repositories"

startDelayOption :: Option
startDelayOption = Option.field [] "startdelay" paramNumber "delay before running startup scan"

seek :: [CommandSeek]
seek = [withFlag Command.Watch.stopOption $ \stopdaemon ->
	withFlag Command.Watch.foregroundOption $ \foreground ->
	withFlag autoStartOption $ \autostart ->
	withField startDelayOption (pure . maybe Nothing parseDuration) $ \startdelay -> 
	withNothing $ start foreground stopdaemon autostart startdelay]

start :: Bool -> Bool -> Bool -> Maybe Duration -> CommandStart
start foreground stopdaemon autostart startdelay
	| autostart = do
		liftIO $ autoStart startdelay
		stop
	| otherwise = do
		ensureInitialized
		Command.Watch.start True foreground stopdaemon startdelay

{- Run outside a git repository. Check to see if any parameter is
 - --autostart and enter autostart mode. -}
checkAutoStart :: IO ()
checkAutoStart = ifM (elem "--autostart" <$> getArgs)
	( autoStart Nothing
	, error "Not in a git repository."
	) 

autoStart :: Maybe Duration -> IO ()
autoStart startdelay = do
	dirs <- liftIO readAutoStartFile
	when (null dirs) $ do
		f <- autoStartFile
		error $ "Nothing listed in " ++ f
	program <- readProgramFile
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

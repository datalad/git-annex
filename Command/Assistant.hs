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

import System.Environment

def :: [Command]
def = [noRepo checkAutoStart $ dontCheck repoExists $
	withOptions [Command.Watch.foregroundOption, Command.Watch.stopOption, autoStartOption] $ 
	command "assistant" paramNothing seek SectionCommon
		"automatically handle changes"]

autoStartOption :: Option
autoStartOption = Option.flag [] "autostart" "start in known repositories"

seek :: [CommandSeek]
seek = [withFlag Command.Watch.stopOption $ \stopdaemon ->
	withFlag Command.Watch.foregroundOption $ \foreground ->
	withFlag autoStartOption $ \autostart ->
	withNothing $ start foreground stopdaemon autostart]

start :: Bool -> Bool -> Bool -> CommandStart
start foreground stopdaemon autostart
	| autostart = do
		liftIO autoStart
		stop
	| otherwise = do
		ensureInitialized
		Command.Watch.start True foreground stopdaemon

{- Run outside a git repository. Check to see if any parameter is
 - --autostart and enter autostart mode. -}
checkAutoStart :: IO ()
checkAutoStart = ifM (elem "--autostart" <$> getArgs)
	( autoStart
	, error "Not in a git repository."
	) 

autoStart :: IO ()
autoStart = do
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
			then boolSystem "ionice" [Param "-c3", Param program, Param "assistant"]
			else boolSystem program [Param "assistant"]

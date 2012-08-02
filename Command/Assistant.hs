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
import Locations.UserConfig

import System.Environment
import System.Posix.Directory

def :: [Command]
def = [noRepo checkAutoStart $ dontCheck repoExists $
	withOptions [Command.Watch.foregroundOption, Command.Watch.stopOption, autoStartOption] $ 
	command "assistant" paramNothing seek "automatically handle changes"]

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
		liftIO $ autoStart
		stop
	| otherwise = do
		ensureInitialized
		Command.Watch.start True foreground stopdaemon

{- Run outside a git repository. Check to see if any parameter is
 - --autostart and enter autostart mode. -}
checkAutoStart :: IO ()
checkAutoStart = ifM (any (== "--autostart") <$> getArgs)
	( autoStart
	, error "Not in a git repository."
	) 

autoStart :: IO ()
autoStart = do
	autostartfile <- autoStartFile
	let nothing = error $ "Nothing listed in " ++ autostartfile
	ifM (doesFileExist autostartfile)
		( do
			dirs <- lines <$> readFile autostartfile
			programfile <- programFile
			program <- catchDefaultIO (readFile programfile) "git-annex"
			when (null dirs) nothing
			forM_ dirs $ \d -> do
				putStrLn $ "git-annex autostart in " ++ d
				ifM (catchBoolIO $ go program d)
					( putStrLn "ok"
					, putStrLn "failed"
					)
		, nothing
		)
	where
		go program dir = do
			changeWorkingDirectory dir
			boolSystem program [Param "assistant"]

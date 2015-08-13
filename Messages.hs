{- git-annex output messages
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages (
	showStart,
	showStart',
	showNote,
	showAction,
	showSideAction,
	doSideAction,
	doQuietSideAction,
	showStoringStateAction,
	showOutput,
	showLongNote,
	showEndOk,
	showEndFail,
	showEndResult,
	endResult,
	toplevelWarning,
	warning,
	warningIO,
	indent,
	maybeShowJSON,
	showFullJSON,
	showCustom,
	showHeader,
	showRaw,
	setupConsole,
	enableDebugOutput,
	disableDebugOutput,
	debugEnabled,
	commandProgressDisabled,
) where

import Text.JSON
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple

import Common
import Types
import Types.Messages
import Messages.Internal
import qualified Messages.JSON as JSON
import Types.Key
import qualified Annex

showStart :: String -> FilePath -> Annex ()
showStart command file = handleMessage (JSON.start command $ Just file) $
	flushed $ putStr $ command ++ " " ++ file ++ " "

showStart' :: String -> Key -> Maybe FilePath -> Annex ()
showStart' command key afile = showStart command $
	fromMaybe (key2file key) afile

showNote :: String -> Annex ()
showNote s = handleMessage (JSON.note s) $
	flushed $ putStr $ "(" ++ s ++ ") "

showAction :: String -> Annex ()
showAction s = showNote $ s ++ "..."

showSideAction :: String -> Annex ()
showSideAction m = Annex.getState Annex.output >>= go
  where
	go st
		| sideActionBlock st == StartBlock = do
			p
			let st' = st { sideActionBlock = InBlock }
			Annex.changeState $ \s -> s { Annex.output = st' }
		| sideActionBlock st == InBlock = return ()
		| otherwise = p
	p = handleMessage q $ putStrLn $ "(" ++ m ++ "...)"
			
showStoringStateAction :: Annex ()
showStoringStateAction = showSideAction "recording state in git"

{- Performs an action, supressing showSideAction messages. -}
doQuietSideAction :: Annex a -> Annex a
doQuietSideAction = doSideAction' InBlock

{- Performs an action, that may call showSideAction multiple times.
 - Only the first will be displayed. -}
doSideAction :: Annex a -> Annex a
doSideAction = doSideAction' StartBlock

doSideAction' :: SideActionBlock -> Annex a -> Annex a
doSideAction' b a = do
	o <- Annex.getState Annex.output
	set $ o { sideActionBlock = b }
	set o `after` a
  where
	set o = Annex.changeState $ \s -> s {  Annex.output = o }

{- Make way for subsequent output of a command. -}
showOutput :: Annex ()
showOutput = unlessM commandProgressDisabled $
	handleMessage q $ putStr "\n"

showLongNote :: String -> Annex ()
showLongNote s = handleMessage (JSON.note s) $
	putStrLn $ '\n' : indent s

showEndOk :: Annex ()
showEndOk = showEndResult True

showEndFail :: Annex ()
showEndFail = showEndResult False

showEndResult :: Bool -> Annex ()
showEndResult ok = handleMessage (JSON.end ok) $ putStrLn $ endResult ok

endResult :: Bool -> String
endResult True = "ok"
endResult False = "failed"

toplevelWarning :: Bool -> String -> Annex ()
toplevelWarning makeway s = warning' makeway ("git-annex: " ++ s)

warning :: String -> Annex ()
warning = warning' True . indent

warning' :: Bool -> String -> Annex ()
warning' makeway w = do
	when makeway $
		handleMessage q $ putStr "\n"
	liftIO $ do
		hFlush stdout
		hPutStrLn stderr w

warningIO :: String -> IO ()
warningIO w = do
	putStr "\n"
	hFlush stdout
	hPutStrLn stderr w

indent :: String -> String
indent = intercalate "\n" . map (\l -> "  " ++ l) . lines

{- Shows a JSON fragment only when in json mode. -}
maybeShowJSON :: JSON a => [(String, a)] -> Annex ()
maybeShowJSON v = handleMessage (JSON.add v) q

{- Shows a complete JSON value, only when in json mode. -}
showFullJSON :: JSON a => [(String, a)] -> Annex Bool
showFullJSON v = withOutputType $ liftIO . go
  where
	go JSONOutput = JSON.complete v >> return True
	go _ = return False

{- Performs an action that outputs nonstandard/customized output, and
 - in JSON mode wraps its output in JSON.start and JSON.end, so it's
 - a complete JSON document.
 - This is only needed when showStart and showEndOk is not used. -}
showCustom :: String -> Annex Bool -> Annex ()
showCustom command a = do
	handleMessage (JSON.start command Nothing) q
	r <- a
	handleMessage (JSON.end r) q

showHeader :: String -> Annex ()
showHeader h = handleMessage q $
	flushed $ putStr $ h ++ ": "

showRaw :: String -> Annex ()
showRaw s = handleMessage q $ putStrLn s

setupConsole :: IO ()
setupConsole = do
	s <- setFormatter
		<$> streamHandler stderr DEBUG
		<*> pure preciseLogFormatter
	updateGlobalLogger rootLoggerName (setLevel NOTICE . setHandlers [s])
	{- This avoids ghc's output layer crashing on
	 - invalid encoded characters in
	 - filenames when printing them out. -}
	fileEncoding stdout
	fileEncoding stderr

{- Log formatter with precision into fractions of a second. -}
preciseLogFormatter :: LogFormatter a
preciseLogFormatter = tfLogFormatter "%F %X%Q" "[$time] $msg"

enableDebugOutput :: IO ()
enableDebugOutput = updateGlobalLogger rootLoggerName $ setLevel DEBUG

disableDebugOutput :: IO ()
disableDebugOutput = updateGlobalLogger rootLoggerName $ setLevel NOTICE

{- Checks if debugging is enabled. -}
debugEnabled :: IO Bool
debugEnabled = do
	l <- getRootLogger
	return $ getLevel l <= Just DEBUG

{- Should commands that normally output progress messages have that
 - output disabled? -}
commandProgressDisabled :: Annex Bool
commandProgressDisabled = withOutputType $ \t -> return $ case t of
	QuietOutput -> True
	ParallelOutput _ -> True
	JSONOutput -> True
	NormalOutput -> False

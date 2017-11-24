{- git-annex output messages
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages (
	showStart,
	ActionItem,
	mkActionItem,
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
	earlyWarning,
	warningIO,
	indent,
	JSON.JSONChunk(..),
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
	outputMessage,
	implicitMessage,
	withMessageState,
	prompt,
) where

import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import Control.Concurrent

import Common
import Types
import Types.Messages
import Types.ActionItem
import Types.Concurrency
import Messages.Internal
import Messages.Concurrent
import qualified Messages.JSON as JSON
import qualified Annex

showStart :: String -> FilePath -> Annex ()
showStart command file = outputMessage json $
	command ++ " " ++ file ++ " "
  where
	json = JSON.start command (Just file) Nothing

showStart' :: String -> Key -> ActionItem -> Annex ()
showStart' command key i = outputMessage json $
	command ++ " " ++ actionItemDesc i key ++ " "
  where
	json = JSON.start command (actionItemWorkTreeFile i) (Just key)

showNote :: String -> Annex ()
showNote s = outputMessage (JSON.note s) $ "(" ++ s ++ ") "

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
	p = outputMessage JSON.none $ "(" ++ m ++ "...)\n"
			
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
	outputMessage JSON.none "\n"

showLongNote :: String -> Annex ()
showLongNote s = outputMessage (JSON.note s) ('\n' : indent s ++ "\n")

showEndOk :: Annex ()
showEndOk = showEndResult True

showEndFail :: Annex ()
showEndFail = showEndResult False

showEndResult :: Bool -> Annex ()
showEndResult ok = outputMessage (JSON.end ok) $ endResult ok ++ "\n"

endResult :: Bool -> String
endResult True = "ok"
endResult False = "failed"

toplevelWarning :: Bool -> String -> Annex ()
toplevelWarning makeway s = warning' makeway ("git-annex: " ++ s)

warning :: String -> Annex ()
warning = warning' True . indent

earlyWarning :: String -> Annex ()
earlyWarning = warning' False

warning' :: Bool -> String -> Annex ()
warning' makeway w = do
	when makeway $
		outputMessage JSON.none "\n"
	outputError (w ++ "\n")

{- Not concurrent output safe. -}
warningIO :: String -> IO ()
warningIO w = do
	putStr "\n"
	hFlush stdout
	hPutStrLn stderr w

indent :: String -> String
indent = intercalate "\n" . map (\l -> "  " ++ l) . lines

{- Shows a JSON chunk only when in json mode. -}
maybeShowJSON :: JSON.JSONChunk v -> Annex ()
maybeShowJSON v = void $ withMessageState $ outputJSON (JSON.add v)

{- Shows a complete JSON value, only when in json mode. -}
showFullJSON :: JSON.JSONChunk v -> Annex Bool
showFullJSON v = withMessageState $ outputJSON (JSON.complete v)

{- Performs an action that outputs nonstandard/customized output, and
 - in JSON mode wraps its output in JSON.start and JSON.end, so it's
 - a complete JSON document.
 - This is only needed when showStart and showEndOk is not used.
 -}
showCustom :: String -> Annex Bool -> Annex ()
showCustom command a = do
	outputMessage (JSON.start command Nothing Nothing) ""
	r <- a
	outputMessage (JSON.end r) ""

showHeader :: String -> Annex ()
showHeader h = outputMessage JSON.none $ (h ++ ": ")

showRaw :: String -> Annex ()
showRaw s = outputMessage JSON.none (s ++ "\n")

setupConsole :: IO ()
setupConsole = do
	s <- setFormatter
		<$> streamHandler stderr DEBUG
		<*> pure preciseLogFormatter
	updateGlobalLogger rootLoggerName (setLevel NOTICE . setHandlers [s])
	{- Force output to be line buffered. This is normally the case when
	 - it's connected to a terminal, but may not be when redirected to
	 - a file or a pipe. -}
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

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
commandProgressDisabled = withMessageState $ \s -> return $
	case outputType s of
		QuietOutput -> True
		JSONOutput _ -> True
		NormalOutput -> concurrentOutputEnabled s

{- Use to show a message that is displayed implicitly, and so might be
 - disabled when running a certian command that needs more control over its
 - output. -}
implicitMessage :: Annex () -> Annex ()
implicitMessage = whenM (implicitMessages <$> Annex.getState Annex.output)

{- Prevents any concurrent console access while running an action, so
 - that the action is the only thing using the console, and can eg prompt
 - the user.
 -}
prompt :: Annex a -> Annex a
prompt a = go =<< Annex.getState Annex.concurrency
  where
	go NonConcurrent = a
	go (Concurrent {}) = withMessageState $ \s -> do
		let l = promptLock s
		bracketIO
			(takeMVar l)
			(putMVar l)
			(const $ hideRegionsWhile a)

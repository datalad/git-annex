{- git-annex output messages
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages (
	showStart,
	showNote,
	showAction,
	showProgress,
	metered,
	meteredBytes,
	showSideAction,
	doSideAction,
	doQuietSideAction,
	showStoringStateAction,
	showOutput,
	showLongNote,
	showEndOk,
	showEndFail,
	showEndResult,
	showErr,
	warning,
	warningIO,
	fileNotFound,
	indent,
	maybeShowJSON,
	showFullJSON,
	showCustom,
	showHeader,
	showRaw,
	setupConsole,
	enableDebugOutput,
	disableDebugOutput
) where

import Text.JSON
import Data.Progress.Meter
import Data.Progress.Tracker
import Data.Quantity
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Handler.Simple
import qualified Data.Set as S

import Common
import Types
import Types.Messages
import qualified Messages.JSON as JSON
import Types.Key
import qualified Annex
import Utility.Metered

showStart :: String -> String -> Annex ()
showStart command file = handle (JSON.start command $ Just file) $
	flushed $ putStr $ command ++ " " ++ file ++ " "

showNote :: String -> Annex ()
showNote s = handle (JSON.note s) $
	flushed $ putStr $ "(" ++ s ++ ") "

showAction :: String -> Annex ()
showAction s = showNote $ s ++ "..."

{- Progress dots. -}
showProgress :: Annex ()
showProgress = handle q $
	flushed $ putStr "."

{- Shows a progress meter while performing a transfer of a key.
 - The action is passed a callback to use to update the meter. -}
metered :: Maybe MeterUpdate -> Key -> (MeterUpdate -> Annex a) -> Annex a
metered combinemeterupdate key a = go (keySize key)
  where
	go (Just size) = meteredBytes combinemeterupdate size a
	go _ = a (const noop)

{- Shows a progress meter while performing an action on a given number
 - of bytes. -}
meteredBytes :: Maybe MeterUpdate -> Integer -> (MeterUpdate -> Annex a) -> Annex a
meteredBytes combinemeterupdate size a = withOutputType go
  where
	go NormalOutput = do
		progress <- liftIO $ newProgress "" size
		meter <- liftIO $ newMeter progress "B" 25 (renderNums binaryOpts 1)
		showOutput
		r <- a $ \n -> liftIO $ do
			setP progress $ fromBytesProcessed n
			displayMeter stdout meter
			maybe noop (\m -> m n) combinemeterupdate
		liftIO $ clearMeter stdout meter
		return r
	go _ = a (const noop)

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
	p = handle q $ putStrLn $ "(" ++ m ++ "...)"
			
showStoringStateAction :: Annex ()
showStoringStateAction = showSideAction "Recording state in git"

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

showOutput :: Annex ()
showOutput = handle q $
	putStr "\n"

showLongNote :: String -> Annex ()
showLongNote s = handle (JSON.note s) $
	putStrLn $ '\n' : indent s

showEndOk :: Annex ()
showEndOk = showEndResult True

showEndFail :: Annex ()
showEndFail = showEndResult False

showEndResult :: Bool -> Annex ()
showEndResult ok = handle (JSON.end ok) $ putStrLn msg
  where
	msg
		| ok = "ok"
		| otherwise = "failed"

showErr :: (Show a) => a -> Annex ()
showErr e = warning' $ "git-annex: " ++ show e

warning :: String -> Annex ()
warning = warning' . indent

warning' :: String -> Annex ()
warning' w = do
	handle q $ putStr "\n"
	liftIO $ do
		hFlush stdout
		hPutStrLn stderr w

warningIO :: String -> IO ()
warningIO w = do
	putStr "\n"
	hFlush stdout
	hPutStrLn stderr w

{- Displays a warning one time about a file the user specified not existing. -}
fileNotFound :: FilePath -> Annex ()
fileNotFound file = do
	st <- Annex.getState Annex.output
	let shown = fileNotFoundShown st
	when (S.notMember file shown) $ do
		let shown' = S.insert file shown
		let st' = st { fileNotFoundShown = shown' }
		Annex.changeState $ \s -> s { Annex.output = st' }
		liftIO $ hPutStrLn stderr $ unwords
			[ "git-annex:", file, "not found" ]

indent :: String -> String
indent = intercalate "\n" . map (\l -> "  " ++ l) . lines

{- Shows a JSON fragment only when in json mode. -}
maybeShowJSON :: JSON a => [(String, a)] -> Annex ()
maybeShowJSON v = handle (JSON.add v) q

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
	handle (JSON.start command Nothing) q
	r <- a
	handle (JSON.end r) q

showHeader :: String -> Annex ()
showHeader h = handle q $
	flushed $ putStr $ h ++ ": "

showRaw :: String -> Annex ()
showRaw s = handle q $ putStrLn s

setupConsole :: IO ()
setupConsole = do
	s <- setFormatter
		<$> streamHandler stderr DEBUG
		<*> pure (simpleLogFormatter "[$time] $msg")
	updateGlobalLogger rootLoggerName (setLevel NOTICE . setHandlers [s])
	{- This avoids ghc's output layer crashing on
	 - invalid encoded characters in
	 - filenames when printing them out. -}
	fileEncoding stdout
	fileEncoding stderr

enableDebugOutput :: IO ()
enableDebugOutput = updateGlobalLogger rootLoggerName $ setLevel DEBUG

disableDebugOutput :: IO ()
disableDebugOutput = updateGlobalLogger rootLoggerName $ setLevel NOTICE

handle :: IO () -> IO () -> Annex ()
handle json normal = withOutputType go
  where
	go NormalOutput = liftIO normal
	go QuietOutput = q
	go JSONOutput = liftIO $ flushed json

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

withOutputType :: (OutputType -> Annex a) -> Annex a
withOutputType a = outputType <$> Annex.getState Annex.output >>= a

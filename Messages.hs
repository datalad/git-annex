{- git-annex output messages
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns, CPP #-}

module Messages (
	showStartMessage,
	showEndMessage,
	StartMessage(..),
	ActionItem(..),
	mkActionItem,
	showNote,
	showAction,
	showSideAction,
	doSideAction,
	doQuietSideAction,
	doQuietAction,
	showStoringStateAction,
	showOutput,
	showLongNote,
	showInfo,
	showEndOk,
	showEndFail,
	showEndResult,
	endResult,
	MessageId(..),
	toplevelFileProblem,
	toplevelWarning,
	warning,
	earlyWarning,
	warningIO,
	indent,
	JSON.JSONChunk(..),
	maybeShowJSON,
	maybeShowJSON',
	maybeAddJSONField,
	showFullJSON,
	showCustom,
	showHeader,
	showRaw,
	setupConsole,
	enableDebugOutput,
	commandProgressDisabled,
	commandProgressDisabled',
	jsonOutputEnabled,
	outputMessage,
	withMessageState,
	MessageState,
	explain,
	prompt,
	mkPrompter,
	sanitizeTopLevelExceptionMessages,
	countdownToMessage,
	enableNormalOutput,
) where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import System.Exit
import qualified Control.Monad.Catch as M
import Data.String

import Common
import Types
import Types.Messages
import Types.ActionItem
import Types.Concurrency
import Types.Command (StartMessage(..), SeekInput)
import Messages.Internal
import Messages.Concurrent
import Annex.Debug
import Annex.Concurrent.Utility
import Utility.SafeOutput
import Git.Quote
import qualified Messages.JSON as JSON
import qualified Annex

showStartMessage :: StartMessage -> Annex ()
showStartMessage (StartMessage command ai si) =
	outputMessage json id $
		UnquotedString command <> " " <> actionItemDesc ai <> " "
  where
	json = JSON.startActionItem command ai si
showStartMessage (StartUsualMessages command ai si) = do
	enableNormalOutput
	showStartMessage (StartMessage command ai si)
showStartMessage (StartNoMessage _) = noop
showStartMessage (CustomOutput _) =
	outputType <$> Annex.getState Annex.output >>= \case
		NormalOutput -> Annex.setOutput QuietOutput
		_ -> noop

-- Only show end result if the StartMessage is one that gets displayed.
showEndMessage :: StartMessage -> Bool -> Annex ()
showEndMessage (StartMessage _ _ _) = showEndResult
showEndMessage (StartUsualMessages _ _ _) = showEndResult
showEndMessage (StartNoMessage _) = const noop
showEndMessage (CustomOutput _) = const noop

showNote :: StringContainingQuotedPath -> Annex ()
showNote s = outputMessage (JSON.note (decodeBS (noquote s))) id $ "(" <> s <> ") "

showAction :: StringContainingQuotedPath -> Annex ()
showAction s = showNote $ s <> "..."

showSideAction :: StringContainingQuotedPath -> Annex ()
showSideAction m = Annex.getState Annex.output >>= go
  where
	go st
		| sideActionBlock st == StartBlock = do
			go'
			let st' = st { sideActionBlock = InBlock }
			Annex.changeState $ \s -> s { Annex.output = st' }
		| sideActionBlock st == InBlock = return ()
		| otherwise = go'
	go' = outputMessage JSON.none id $ "(" <> m <> "...)\n"
			
showStoringStateAction :: Annex ()
showStoringStateAction = showSideAction "recording state in git"

{- Performs an action, suppressing showSideAction messages. -}
doQuietSideAction :: Annex a -> Annex a
doQuietSideAction = doSideAction' InBlock

{- Performs an action, that may call showSideAction multiple times.
 - Only the first will be displayed. -}
doSideAction :: Annex a -> Annex a
doSideAction = doSideAction' StartBlock

doSideAction' :: SideActionBlock -> Annex a -> Annex a
doSideAction' b = bracket setup cleanup . const
  where
	setup = do
		o <- Annex.getState Annex.output
		set $ o { sideActionBlock = b }
		return o
	cleanup = set
	set o = Annex.changeState $ \s -> s { Annex.output = o }

{- Performs an action, suppressing all normal standard output,
 - but not json output. -}
doQuietAction :: Annex a -> Annex a
doQuietAction = bracket setup cleanup . const
  where
	setup = do
		o <- Annex.getState Annex.output
		case outputType o of
			NormalOutput -> set $ o { outputType = QuietOutput }
			_ -> noop
		return o
	cleanup = set
	set o = Annex.changeState $ \s -> s {  Annex.output = o }

{- Make way for subsequent output of a command. -}
showOutput :: Annex ()
showOutput = unlessM commandProgressDisabled $
	outputMessage JSON.none id "\n"

showLongNote :: StringContainingQuotedPath -> Annex ()
showLongNote s = outputMessage (JSON.note (decodeBS (noquote s))) formatLongNote s

formatLongNote :: S.ByteString -> S.ByteString
formatLongNote s = "\n" <> indent s <> "\n"

-- Used by external special remote, displayed same as showLongNote
-- to console, but json object containing the info is emitted immediately.
showInfo :: StringContainingQuotedPath -> Annex ()
showInfo s = outputMessage' outputJSON (JSON.info (decodeBS (noquote s))) formatLongNote s

showEndOk :: Annex ()
showEndOk = showEndResult True

showEndFail :: Annex ()
showEndFail = showEndResult False

showEndResult :: Bool -> Annex ()
showEndResult ok = outputMessage (JSON.end ok) id $
	UnquotedByteString (endResult ok) <> "\n"

endResult :: Bool -> S.ByteString
endResult True = "ok"
endResult False = "failed"

toplevelMsg :: (Semigroup t, IsString t) => t -> t
toplevelMsg s = fromString "git-annex: " <> s

toplevelFileProblem :: Bool -> MessageId -> StringContainingQuotedPath -> String -> OsPath -> Maybe Key -> SeekInput -> Annex ()
toplevelFileProblem makeway messageid msg action file mkey si = do
	maybeShowJSON' $ JSON.start action (Just file) mkey si
	maybeShowJSON' $ JSON.messageid messageid
	warning' makeway id (toplevelMsg (QuotedPath file <> " " <> msg))
	maybeShowJSON' $ JSON.end False

toplevelWarning :: Bool -> StringContainingQuotedPath -> Annex ()
toplevelWarning makeway s = warning' makeway id (toplevelMsg s)

warning :: StringContainingQuotedPath -> Annex ()
warning = warning' True indent

earlyWarning :: StringContainingQuotedPath -> Annex ()
earlyWarning = warning' False id

warning' :: Bool -> (S.ByteString -> S.ByteString) -> StringContainingQuotedPath -> Annex ()
warning' makeway consolewhitespacef msg = do
	when makeway $
		outputMessage JSON.none id "\n"
	outputError (\s -> consolewhitespacef s <> "\n") msg

{- Not concurrent output safe. -}
warningIO :: String -> IO ()
warningIO w = do
	putStr "\n"
	hFlush stdout
	hPutStrLn stderr (safeOutput w)

indent :: S.ByteString -> S.ByteString
indent = S.intercalate "\n" . map ("  " <>) . S8.lines

{- Shows a JSON chunk only when in json mode. -}
maybeShowJSON :: JSON.JSONChunk v -> Annex ()
maybeShowJSON v = void $ withMessageState $ bufferJSON (JSON.add v)

maybeShowJSON' :: JSON.JSONBuilder -> Annex ()
maybeShowJSON' v = void $ withMessageState $ bufferJSON v

{- Adds a field to the current json object. -}
maybeAddJSONField :: JSON.ToJSON' v => String -> v -> Annex ()
maybeAddJSONField f v = case JSON.toJSON' (JSON.AddJSONActionItemField f v) of
	JSON.Object o -> maybeShowJSON $ JSON.AesonObject o
	_ -> noop

{- Shows a complete JSON value, only when in json mode. -}
showFullJSON :: JSON.JSONChunk v -> Annex Bool
showFullJSON v = withMessageState $ bufferJSON (JSON.complete v)

{- Performs an action that outputs nonstandard/customized output, and
 - in JSON mode wraps its output in JSON.start and JSON.end, so it's
 - a complete JSON document.
 - This is only needed when showStartMessage and showEndOk is not used.
 -}
showCustom :: String -> SeekInput -> Annex Bool -> Annex ()
showCustom command si a = do
	outputMessage (JSON.start command Nothing Nothing si) id ""
	r <- a
	outputMessage (JSON.end r) id ""

showHeader :: S.ByteString -> Annex ()
showHeader h = outputMessage JSON.none id (UnquotedByteString h <> ": ")

showRaw :: S.ByteString -> Annex ()
showRaw s = outputMessage JSON.none id (UnquotedByteString s <> "\n")

setupConsole :: IO ()
setupConsole = do
	dd <- debugDisplayer
	configureDebug dd (DebugSelector (const False))
	{- Force output to be line buffered. This is normally the case when
	 - it's connected to a terminal, but may not be when redirected to
	 - a file or a pipe. -}
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering
#ifdef mingw32_HOST_OS
	{- Avoid outputting CR at end of line on Windows. git commands do
	 - not output CR there. -}
	hSetNewlineMode stdout noNewlineTranslation
	hSetNewlineMode stderr noNewlineTranslation
#endif

enableDebugOutput :: Annex ()
enableDebugOutput = do
	selector <- Annex.getRead Annex.debugselector
	dd <- liftIO debugDisplayer
	liftIO $ configureDebug dd selector

debugDisplayer :: IO (S.ByteString -> IO ())
debugDisplayer = do
	-- Debug output will get mixed in with any other output
	-- made by git-annex, but use a lock to prevent two debug lines
	-- that are displayed at the same time from mixing together.
	lock <- newMVar ()
	return $ \s -> withMVar lock $ \() -> do
		S.hPutStr stderr (safeOutput s <> "\n")
		hFlush stderr

{- Should commands that normally output progress messages have that
 - output disabled? -}
commandProgressDisabled :: Annex Bool
commandProgressDisabled = withMessageState $ return . commandProgressDisabled'

commandProgressDisabled' :: MessageState -> Bool
commandProgressDisabled' s = case outputType s of
	NormalOutput -> concurrentOutputEnabled s
	QuietOutput -> True
	JSONOutput _ -> True
	SerializedOutput _ _ -> True

jsonOutputEnabled :: Annex Bool
jsonOutputEnabled = withMessageState $ \s -> return $
	case outputType s of
		JSONOutput _ -> True
		_ -> False

explain :: ActionItem -> Maybe StringContainingQuotedPath -> Annex ()
explain ai (Just msg) = do
	rd <- Annex.getRead id
	let d = actionItemDesc ai
	let msg' = "[ " <> (if d == mempty then "" else (d <> " ")) <> msg <> " ]\n"
	if Annex.explainenabled rd
		then outputMessage JSON.none id msg'
		else fastDebug' rd "Messages.explain" (decodeBS (noquote msg'))
explain _ _ = return ()

{- Prevents any concurrent console access while running an action, so
 - that the action is the only thing using the console, and can eg prompt
 - the user.
 -}
prompt :: Annex a -> Annex a
prompt a = do
	p <- mkPrompter
	p a

{- Like prompt, but for a non-annex action that prompts. -}
mkPrompter :: (MonadMask m, MonadIO m) => Annex (m a -> m a)
mkPrompter = getConcurrency >>= \case
	NonConcurrent -> return id
	(Concurrent _) -> goconcurrent
	ConcurrentPerCpu -> goconcurrent
  where
	goconcurrent = withMessageState $ \s -> do
		let l = promptLock s
		let (run, cleanup) = case outputType s of
			SerializedOutput h hr ->
				( \a -> do
					liftIO $ outputSerialized h BeginPrompt
					liftIO $ waitOutputSerializedResponse hr ReadyPrompt
					a
				, liftIO $ outputSerialized h EndPrompt
				)
			_ ->
				( hideRegionsWhile s
				, noop
				)
		return $ \a ->
			debugLocks $ bracketIO
				(takeMVar l)
				(\v -> putMVar l v >> cleanup)
				(const $ run a)

{- Catch all (non-async and not ExitCode) exceptions and display, 
 - sanitizing any control characters in the exceptions.
 -
 - Exits nonzero on exception, so should only be used at topmost level.
 -}
sanitizeTopLevelExceptionMessages :: IO a -> IO a
sanitizeTopLevelExceptionMessages a = a `catches`
	((M.Handler (\ (e :: ExitCode) -> throwM e)) : nonAsyncHandler go)
  where
	go e = do
		hPutStrLn stderr $ safeOutput $ toplevelMsg (show e)
		exitWith $ ExitFailure 1

{- Used to only run an action that displays a message after the specified
 - number of steps. This is useful when performing an action that can
 - sometimes take a long time, but often does not.
 -}
countdownToMessage :: Int -> Annex () -> Annex Int
countdownToMessage n showmsg
	| n < 1 = return 0
	| n == 1 = do
		showmsg
		return 0
	| otherwise = do
		let !n' = pred n
		return n'

enableNormalOutput :: Annex ()
enableNormalOutput =
	outputType <$> Annex.getState Annex.output >>= \case
		QuietOutput -> Annex.setOutput NormalOutput
		_ -> noop

{- git-annex output messages
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Messages (
	showStart,
	showStartOther,
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
	commandProgressDisabled,
	commandProgressDisabled',
	jsonOutputEnabled,
	outputMessage,
	withMessageState,
	MessageState,
	prompt,
	mkPrompter,
) where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.ByteString as S

import Common
import Types
import Types.Messages
import Types.ActionItem
import Types.Concurrency
import Types.Command (StartMessage(..), SeekInput)
import Types.Transfer (transferKey)
import Messages.Internal
import Messages.Concurrent
import Annex.Debug
import Annex.Concurrent.Utility
import qualified Messages.JSON as JSON
import qualified Annex

showStart :: String -> RawFilePath -> SeekInput -> Annex ()
showStart command file si = outputMessage json $
	encodeBS command <> " " <> file <> " "
  where
	json = JSON.start command (Just file) Nothing si

showStartKey :: String -> Key -> ActionItem -> SeekInput -> Annex ()
showStartKey command key ai si = outputMessage json $
	encodeBS command <> " " <> actionItemDesc ai <> " "
  where
	json = JSON.start command (actionItemFile ai) (Just key) si

showStartOther :: String -> Maybe String -> SeekInput -> Annex ()
showStartOther command mdesc si = outputMessage json $ encodeBS $
	command ++ (maybe "" (" " ++) mdesc) ++ " "
  where
	json = JSON.start command Nothing Nothing si

showStartMessage :: StartMessage -> Annex ()
showStartMessage (StartMessage command ai si) = case ai of
	ActionItemAssociatedFile _ k -> showStartKey command k ai si
	ActionItemKey k -> showStartKey command k ai si
	ActionItemBranchFilePath _ k -> showStartKey command k ai si
	ActionItemFailedTransfer t _ -> showStartKey command (transferKey t) ai si
	ActionItemTreeFile file -> showStart command file si
	ActionItemOther msg -> showStartOther command msg si
	OnlyActionOn _ ai' -> showStartMessage (StartMessage command ai' si)
showStartMessage (StartUsualMessages command ai si) = do
	outputType <$> Annex.getState Annex.output >>= \case
		QuietOutput -> Annex.setOutput NormalOutput
		_ -> noop
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

showNote :: String -> Annex ()
showNote s = outputMessage (JSON.note s) $ encodeBS $ "(" ++ s ++ ") "

showAction :: String -> Annex ()
showAction s = showNote $ s ++ "..."

showSideAction :: String -> Annex ()
showSideAction m = Annex.getState Annex.output >>= go
  where
	go st
		| sideActionBlock st == StartBlock = do
			go'
			let st' = st { sideActionBlock = InBlock }
			Annex.changeState $ \s -> s { Annex.output = st' }
		| sideActionBlock st == InBlock = return ()
		| otherwise = go'
	go' = outputMessage JSON.none $ encodeBS $ "(" ++ m ++ "...)\n"
			
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
	outputMessage JSON.none "\n"

showLongNote :: String -> Annex ()
showLongNote s = outputMessage (JSON.note s) (encodeBS (formatLongNote s))

formatLongNote :: String -> String
formatLongNote s = '\n' : indent s ++ "\n"

-- Used by external special remote, displayed same as showLongNote
-- to console, but json object containing the info is emitted immediately.
showInfo :: String -> Annex ()
showInfo s = outputMessage' outputJSON (JSON.info s) $
	encodeBS (formatLongNote s)

showEndOk :: Annex ()
showEndOk = showEndResult True

showEndFail :: Annex ()
showEndFail = showEndResult False

showEndResult :: Bool -> Annex ()
showEndResult ok = outputMessage (JSON.end ok) $ endResult ok <> "\n"

endResult :: Bool -> S.ByteString
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
maybeShowJSON v = void $ withMessageState $ bufferJSON (JSON.add v)

{- Shows a complete JSON value, only when in json mode. -}
showFullJSON :: JSON.JSONChunk v -> Annex Bool
showFullJSON v = withMessageState $ bufferJSON (JSON.complete v)

{- Performs an action that outputs nonstandard/customized output, and
 - in JSON mode wraps its output in JSON.start and JSON.end, so it's
 - a complete JSON document.
 - This is only needed when showStart and showEndOk is not used.
 -}
showCustom :: String -> SeekInput -> Annex Bool -> Annex ()
showCustom command si a = do
	outputMessage (JSON.start command Nothing Nothing si) ""
	r <- a
	outputMessage (JSON.end r) ""

showHeader :: S.ByteString -> Annex ()
showHeader h = outputMessage JSON.none (h <> ": ")

showRaw :: S.ByteString -> Annex ()
showRaw s = outputMessage JSON.none (s <> "\n")

setupConsole :: IO ()
setupConsole = do
	dd <- debugDisplayer
	configureDebug dd (DebugSelector (const False))
	{- Force output to be line buffered. This is normally the case when
	 - it's connected to a terminal, but may not be when redirected to
	 - a file or a pipe. -}
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

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
		S.hPutStr stderr (s <> "\n")
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

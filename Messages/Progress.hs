{- git-annex progress output
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Messages.Progress where

import Common
import Messages
import Utility.Metered
import Types
import Types.Messages
import Types.Key
import qualified Messages.JSON as JSON

#ifdef WITH_CONCURRENTOUTPUT
import Messages.Concurrent
import qualified System.Console.Regions as Regions
import qualified System.Console.Concurrent as Console
#endif

import Data.Progress.Meter
import Data.Progress.Tracker
import Data.Quantity

{- Shows a progress meter while performing a transfer of a key.
 - The action is passed a callback to use to update the meter. -}
metered :: Maybe MeterUpdate -> Key -> (MeterUpdate -> Annex a) -> Annex a
metered othermeter key a = case keySize key of
	Nothing -> nometer
	Just size -> withMessageState (go $ fromInteger size)
  where
	go _ (MessageState { outputType = QuietOutput }) = nometer
	go size (MessageState { outputType = NormalOutput, concurrentOutputEnabled = False }) = do
		showOutput
		(progress, meter) <- mkmeter size
		m <- liftIO $ rateLimitMeterUpdate 0.1 (Just size) $ \n -> do
			setP progress $ fromBytesProcessed n
			displayMeter stdout meter
		r <- a (combinemeter m)
		liftIO $ clearMeter stdout meter
		return r
	go size (MessageState { outputType = NormalOutput, concurrentOutputEnabled = True }) =
#if WITH_CONCURRENTOUTPUT
		withProgressRegion $ \r -> do
			(progress, meter) <- mkmeter size
			m <- liftIO $ rateLimitMeterUpdate 0.1 (Just size) $ \n -> do
				setP progress $ fromBytesProcessed n
				s <- renderMeter meter
				Regions.setConsoleRegion r ("\n" ++ s)
			a (combinemeter m)
#else
		nometer
#endif
	go _ (MessageState { outputType = JSONOutput False }) = nometer
	go size (MessageState { outputType = JSONOutput True }) = do
		buf <- withMessageState $ return . jsonBuffer
		m <- liftIO $ rateLimitMeterUpdate 0.1 (Just size) $
			JSON.progress buf size
		a (combinemeter m)

	mkmeter size = do
		progress <- liftIO $ newProgress "" size
		meter <- liftIO $ newMeter progress "B" 25 (renderNums binaryOpts 1)
		return (progress, meter)

	nometer = a $ combinemeter (const noop)

	combinemeter m = case othermeter of
		Nothing -> m
		Just om -> combineMeterUpdate m om

{- Use when the command's own progress output is preferred.
 - The command's output will be suppressed and git-annex's progress output
 - used for concurrent output, and json progress. -}
commandMetered :: Maybe MeterUpdate -> Key -> (MeterUpdate -> Annex a) -> Annex a
commandMetered combinemeterupdate key a = 
	withMessageState $ \s -> case outputType s of
		JSONOutput True -> usemeter
		NormalOutput | concurrentOutputEnabled s -> usemeter
		_ -> a (fromMaybe nullMeterUpdate combinemeterupdate)
  where
	usemeter = metered combinemeterupdate key a

{- Poll file size to display meter, but only for concurrent output. -}
concurrentMeteredFile :: FilePath -> Maybe MeterUpdate -> Key -> Annex a -> Annex a
concurrentMeteredFile file combinemeterupdate key a = 
	withMessageState $ \s -> if concurrentOutputEnabled s
		then metered combinemeterupdate key $ \p ->
			watchFileSize file p a
		else a

{- Progress dots. -}
showProgressDots :: Annex ()
showProgressDots = outputMessage JSON.none "."

{- Runs a command, that may output progress to either stdout or
 - stderr, as well as other messages.
 -
 - In quiet mode, the output is suppressed, except for error messages.
 -}
progressCommand :: FilePath -> [CommandParam] -> Annex Bool
progressCommand cmd params = progressCommandEnv cmd params Nothing

progressCommandEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> Annex Bool
progressCommandEnv cmd params environ = ifM commandProgressDisabled
	( do
		oh <- mkOutputHandler
		liftIO $ demeterCommandEnv oh cmd params environ
	, liftIO $ boolSystemEnv cmd params environ
	)

mkOutputHandler :: Annex OutputHandler
mkOutputHandler = OutputHandler
	<$> commandProgressDisabled
	<*> mkStderrEmitter

mkStderrRelayer :: Annex (Handle -> IO ())
mkStderrRelayer = do
	quiet <- commandProgressDisabled
	emitter <- mkStderrEmitter
	return $ \h -> avoidProgress quiet h emitter

{- Generates an IO action that can be used to emit stderr.
 -
 - When a progress meter is displayed, this takes care to avoid
 - messing it up with interleaved stderr from a command.
 -}
mkStderrEmitter :: Annex (String -> IO ())
mkStderrEmitter = withMessageState go
  where
#ifdef WITH_CONCURRENTOUTPUT
	go s | concurrentOutputEnabled s = return Console.errorConcurrent
#endif
	go _ = return (hPutStrLn stderr)

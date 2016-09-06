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
import Messages.Internal
import Utility.Metered
import Types
import Types.Messages
import Types.Key

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
metered combinemeterupdate key a = case keySize key of
	Nothing -> nometer
	Just size -> withOutputType (go $ fromInteger size)
  where
	go _ QuietOutput = nometer
	go _ JSONOutput = nometer
	go size NormalOutput = do
		showOutput
		(progress, meter) <- mkmeter size
		r <- a $ \n -> liftIO $ do
			setP progress $ fromBytesProcessed n
			displayMeter stdout meter
			maybe noop (\m -> m n) combinemeterupdate
		liftIO $ clearMeter stdout meter
		return r
#if WITH_CONCURRENTOUTPUT
	go size o@(ConcurrentOutput {})
		| concurrentOutputEnabled o = withProgressRegion $ \r -> do
			(progress, meter) <- mkmeter size
			a $ \n -> liftIO $ do
				setP progress $ fromBytesProcessed n
				s <- renderMeter meter
				Regions.setConsoleRegion r ("\n" ++ s)
				maybe noop (\m -> m n) combinemeterupdate
#else
	go _size _o
#endif
		| otherwise = nometer

	mkmeter size = do
		progress <- liftIO $ newProgress "" size
		meter <- liftIO $ newMeter progress "B" 25 (renderNums binaryOpts 1)
		return (progress, meter)

	nometer = a (const noop)

{- Use when the progress meter is only desired for concurrent
 - output; as when a command's own progress output is preferred. -}
concurrentMetered :: Maybe MeterUpdate -> Key -> (MeterUpdate -> Annex a) -> Annex a
concurrentMetered combinemeterupdate key a = withOutputType go
  where
	go (ConcurrentOutput {}) = metered combinemeterupdate key a
	go _ = a (fromMaybe nullMeterUpdate combinemeterupdate)

{- Poll file size to display meter, but only for concurrent output. -}
concurrentMeteredFile :: FilePath -> Maybe MeterUpdate -> Key -> Annex a -> Annex a
concurrentMeteredFile file combinemeterupdate key a = withOutputType go
  where
	go (ConcurrentOutput {}) = metered combinemeterupdate key $ \p ->
		watchFileSize file p a
	go _ = a

{- Progress dots. -}
showProgressDots :: Annex ()
showProgressDots = outputMessage q "."

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
mkStderrEmitter = withOutputType go
  where
#ifdef WITH_CONCURRENTOUTPUT
	go o | concurrentOutputEnabled o = return Console.errorConcurrent
#endif
	go _ = return (hPutStrLn stderr)

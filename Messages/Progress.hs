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

#ifdef WITH_ASCIIPROGRESS
import System.Console.AsciiProgress
import qualified System.Console.Terminal.Size as Terminal
import Control.Concurrent
#else
import Data.Progress.Meter
import Data.Progress.Tracker
import Data.Quantity
#endif

{- Shows a progress meter while performing a transfer of a key.
 - The action is passed a callback to use to update the meter. -}
metered :: Maybe MeterUpdate -> Key -> AssociatedFile -> (MeterUpdate -> Annex a) -> Annex a
metered combinemeterupdate key af a = case keySize key of
	Nothing -> nometer
	Just size -> withOutputType (go $ fromInteger size)
  where
	go _ QuietOutput = nometer
	go _ JSONOutput = nometer
#ifdef WITH_ASCIIPROGRESS
	go size _ = do
		showOutput
		liftIO $ putStrLn ""

		cols <- liftIO $ maybe 79 Terminal.width <$> Terminal.size
		let desc = truncatepretty cols $ fromMaybe (key2file key) af

		result <- liftIO newEmptyMVar
		pg <- liftIO $ newProgressBar def
			{ pgWidth = cols
			, pgFormat = desc ++ " :percent :bar ETA :eta"
			, pgTotal = size
			, pgOnCompletion = do
				ok <- takeMVar result
				putStrLn $ desc ++ " " ++ endResult ok
			}
		r <- a $ liftIO . pupdate pg

		liftIO $ do
			-- See if the progress bar is complete or not.
			sofar <- stCompleted <$> getProgressStats pg
			putMVar result (sofar >= size)
			-- May not be actually complete if the action failed,
			-- but this just clears the progress bar.
			complete pg

		return r
#else
	-- Old progress bar code, not suitable for parallel output.
	go _ (ParallelOutput _) = do
		r <- nometer
		liftIO $ putStrLn $ fromMaybe (key2file key) af
		return r
	go size NormalOutput = do
		showOutput
		progress <- liftIO $ newProgress "" size
		meter <- liftIO $ newMeter progress "B" 25 (renderNums binaryOpts 1)
		r <- a $ liftIO . pupdate meter progress
		liftIO $ clearMeter stdout meter
		return r
#endif

#ifdef WITH_ASCIIPROGRESS
	pupdate pg n = do
		let i = fromBytesProcessed n
		sofar <- stCompleted <$> getProgressStats pg
		when (i > sofar) $
			tickN pg (i - sofar)
		threadDelay 100
#else
	pupdate meter progress n = do
		setP progress $ fromBytesProcessed n
		displayMeter stdout meter
#endif
		maybe noop (\m -> m n) combinemeterupdate

	nometer = a (const noop)

#ifdef WITH_ASCIIPROGRESS
	truncatepretty n s
		| length s > n = take (n-2) s ++ ".."
		| otherwise = s
#endif

{- Use when the progress meter is only desired for parallel
 - mode; as when a command's own progress output is preferred. -}
parallelMetered :: Maybe MeterUpdate -> Key -> AssociatedFile -> (MeterUpdate -> Annex a) -> Annex a
parallelMetered combinemeterupdate key af a = withOutputType go
  where
	go (ParallelOutput _) = metered combinemeterupdate key af a
	go _ = a (fromMaybe (const noop) combinemeterupdate)

{- Progress dots. -}
showProgressDots :: Annex ()
showProgressDots = handleMessage q $
	flushed $ putStr "."

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
	go (ParallelOutput _) = return $ \s -> hPutStrLn stderr ("E: " ++ s)
	go _ = return (hPutStrLn stderr)

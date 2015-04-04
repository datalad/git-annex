{- git-annex progress output
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages.Progress where

import Common
import Messages
import Messages.Internal
import Utility.Metered
import Types
import Types.Messages
import Types.Key

import System.Console.AsciiProgress
import Control.Concurrent

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
	go QuietOutput = nometer
	go JSONOutput = nometer
	go _ = do
		showOutput
		liftIO $ putStrLn ""
		pg <- liftIO $ newProgressBar def
			{ pgWidth = 79
			, pgFormat = ":percent :bar ETA :eta"
			, pgTotal = fromInteger size
			}
		r <- a $ liftIO . pupdate pg

		-- may not be actually complete if the action failed,
		-- but this just clears the progress bar
		liftIO $ complete pg

		return r

	pupdate pg n = do
		let i = fromBytesProcessed n
		sofar <- stCompleted <$> getProgressStats pg
		when (i > sofar) $
			tickN pg (i - sofar)
		threadDelay 100
		maybe noop (\m -> m n) combinemeterupdate

	nometer = a (const noop)

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
	go ProgressOutput = return $ \s -> hPutStrLn stderr ("E: " ++ s)
	go _ = return (hPutStrLn stderr)

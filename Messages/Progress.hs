module Messages.Progress where

import Common
import Messages
import Messages.Internal
import Utility.Metered
import Types
import Types.Messages
import Types.Key

import Data.Progress.Meter
import Data.Progress.Tracker
import Data.Quantity

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

{- Progress dots. -}
showProgressDots :: Annex ()
showProgressDots = handleMessage q $
	flushed $ putStr "."

{- Runs a command, the output of which is some sort of progress display.
 -
 - Normally, this is displayed to the user.
 -
 - In QuietOutput mode, both the stdout and stderr are discarded,
 - unless the command fails, in which case stderr will be displayed.
 -}
progressOutput :: FilePath -> [CommandParam] -> Annex Bool
progressOutput cmd ps = undefined

mkProgressHandler :: MeterUpdate -> Annex ProgressHandler
mkProgressHandler meter = ProgressHandler
	<$> quietmode
	<*> (stderrhandler <$> mkStderrEmitter)
	<*> pure meter
  where
	quietmode = withOutputType $ \t -> return $ case t of
		ProgressOutput -> True
		_ -> False
	stderrhandler emitter h = do
		void $ emitter =<< hGetLine stderr
		stderrhandler emitter h

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

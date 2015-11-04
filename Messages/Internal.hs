{-# OPTIONS_GHC -fno-warn-orphans #-}

{- git-annex output messages, including concurrent output
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Messages.Internal where

import Common
import Annex
import Types.Messages

#ifdef WITH_CONCURRENTOUTPUT
import qualified System.Console.Concurrent as Console
import qualified System.Console.Regions as Regions
import Data.String
import Control.Concurrent.STM
#endif

withOutputType :: (OutputType -> Annex a) -> Annex a
withOutputType a = outputType <$> Annex.getState Annex.output >>= a

outputMessage :: IO () -> String -> Annex ()
outputMessage json s = withOutputType go
  where
	go NormalOutput = liftIO $
		flushed $ putStr s
	go QuietOutput = q
	go (ConcurrentOutput _) = concurrentMessage False s q
	go JSONOutput = liftIO $ flushed json

outputError :: String -> Annex ()
outputError s = withOutputType go
  where
	go (ConcurrentOutput _) = concurrentMessage True s (go NormalOutput)
	go _ = liftIO $ do
		hFlush stdout
		hPutStr stderr s
		hFlush stderr

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

{- Outputs a message in a concurrency safe way.
 -
 - The message may be an error message, in which case it goes to stderr.
 -
 - When built without concurrent-output support, the fallback action is run
 - instead.
 -}
concurrentMessage :: Bool -> String -> Annex () -> Annex ()
#ifdef WITH_CONCURRENTOUTPUT
concurrentMessage iserror msg _ = go =<< Annex.getState Annex.consoleregion
  where
	go Nothing
		| iserror = liftIO $ Console.errorConcurrent msg
		| otherwise = liftIO $ Console.outputConcurrent msg
	go (Just r) = do
		-- Can't display the error to stdout while
		-- console regions are in use, so set the errflag
		-- to get it to display to stderr later.
		when iserror $
			Annex.changeState $ \s -> s { Annex.consoleregionerrflag = True }
		liftIO $ Regions.appendConsoleRegion r msg
#else
concurrentMessage _ _ fallback = fallback
#endif

{- Enable concurrent output when that has been requested.
 -
 - This should only be run once per git-annex lifetime, with
 - everything that might generate messages run inside it.
 -}
withConcurrentOutput :: Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
withConcurrentOutput a = withOutputType go
  where
	go (ConcurrentOutput _) = Console.withConcurrentOutput a
	go _ = a
#else
withConcurrentOutput = id
#endif

{- Runs an action in its own dedicated region of the console.
 -
 - The region is closed at the end or on exception, and at that point
 - the value of the region is displayed in the scrolling area above
 - any other active regions.
 -
 - When not at a console, a region is not displayed until the end.
 -}
inOwnConsoleRegion :: Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
inOwnConsoleRegion a = Regions.withConsoleRegion Regions.Linear $ \r -> do
	setregion (Just r)
	a `finally` removeregion r
  where
	setregion v = Annex.changeState $ \s -> s { Annex.consoleregion = v }
	removeregion r = do
		errflag <- Annex.getState Annex.consoleregionerrflag
		let h = if errflag then Console.StdErr else Console.StdOut
		Annex.changeState $ \s -> s { Annex.consoleregionerrflag = False }
		setregion Nothing
		liftIO $ atomically $ do
			t <- Regions.getConsoleRegion r
			Regions.closeConsoleRegion r
			Console.bufferOutputSTM h $
				Console.toOutput (t <> fromString "\n")
#else
inOwnConsoleRegion = id
#endif

#ifdef WITH_CONCURRENTOUTPUT
instance Regions.LiftRegion Annex where
	liftRegion = liftIO . atomically
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- git-annex output messages, including concurrent output to display regions
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
import Control.Concurrent.STM
import qualified Data.Text as T
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
concurrentMessage iserror msg _ = go =<< consoleRegion <$> Annex.getState Annex.output
  where
	go Nothing
		| iserror = liftIO $ Console.errorConcurrent msg
		| otherwise = liftIO $ Console.outputConcurrent msg
	go (Just r) = do
		-- Can't display the error to stdout while
		-- console regions are in use, so set the errflag
		-- to get it to display to stderr later.
		when iserror $ do
			Annex.changeState $ \s ->
				s { Annex.output = (Annex.output s) { consoleRegionErrFlag = True } }
		liftIO $ Regions.appendConsoleRegion r msg
#else
concurrentMessage _ _ fallback = fallback
#endif

{- Do concurrent output when that has been requested. -}
allowConcurrentOutput :: Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
allowConcurrentOutput a = go =<< Annex.getState Annex.concurrentjobs
  where
	go (Just n) = Regions.displayConsoleRegions $ bracket_
		(Annex.setOutput (ConcurrentOutput n))
		(Annex.setOutput NormalOutput)
		a
	go Nothing = a
#else
allowConcurrentOutput = id
#endif

{- Runs an action in its own dedicated region of the console.
 -
 - The region is closed at the end or on exception, and at that point
 - the value of the region is displayed in the scrolling area above
 - any other active regions.
 -
 - When not at a console, a region is not displayed until the action is
 - complete.
 -}
inOwnConsoleRegion :: Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
inOwnConsoleRegion a = bracket mkregion rmregion go
  where
	go r = do
		setregion (Just r)
		a
	mkregion = Regions.openConsoleRegion Regions.Linear
	rmregion r = do
		errflag <- consoleRegionErrFlag <$> Annex.getState Annex.output
		let h = if errflag then Console.StdErr else Console.StdOut
		Annex.changeState $ \s ->
			s { Annex.output = (Annex.output s) { consoleRegionErrFlag = False } }
		setregion Nothing
		liftIO $ atomically $ do
			t <- Regions.getConsoleRegion r
			unless (T.null t) $
				Console.bufferOutputSTM h t
			Regions.closeConsoleRegion r
	setregion r = Annex.changeState $ \s -> s { Annex.output = (Annex.output s) { consoleRegion = r } }
#else
inOwnConsoleRegion = id
#endif

#ifdef WITH_CONCURRENTOUTPUT
instance Regions.LiftRegion Annex where
	liftRegion = liftIO . atomically
#endif

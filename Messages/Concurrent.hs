{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Messages.Concurrent where

import Annex

#ifdef WITH_CONCURRENTOUTPUT
import Common
import Types.Messages
import qualified System.Console.Concurrent as Console
import qualified System.Console.Regions as Regions
import Control.Concurrent.STM
import qualified Data.Text as T
import GHC.IO.Encoding
#endif

{- Outputs a message in a concurrency safe way.
 -
 - The message may be an error message, in which case it goes to stderr.
 -
 - When built without concurrent-output support, the fallback action is run
 - instead.
 -}
concurrentMessage :: OutputType -> Bool -> String -> Annex () -> Annex ()
#ifdef WITH_CONCURRENTOUTPUT
concurrentMessage o iserror msg fallback 
	| concurrentOutputEnabled o =
		go =<< consoleRegion <$> Annex.getState Annex.output
#endif
	| otherwise = fallback
#ifdef WITH_CONCURRENTOUTPUT
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
		liftIO $ atomically $ do
			Regions.appendConsoleRegion r msg
			rl <- takeTMVar Regions.regionList
			putTMVar Regions.regionList
				(if r `elem` rl then rl else r:rl)
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
inOwnConsoleRegion :: OutputType -> Annex a -> Annex a
inOwnConsoleRegion o a
#ifdef WITH_CONCURRENTOUTPUT
	| concurrentOutputEnabled o = do
		r <- mkregion
		setregion (Just r)
		eret <- tryNonAsync a `onException` rmregion r
		case eret of
			Left e -> do
				-- Add error message to region before it closes.
				concurrentMessage o True (show e) noop
				rmregion r
				throwM e
			Right ret -> do
				rmregion r
				return ret
#endif
	| otherwise = a
#ifdef WITH_CONCURRENTOUTPUT
  where
	-- The region is allocated here, but not displayed until 
	-- a message is added to it. This avoids unnecessary screen
	-- updates when a region does not turn out to need to be used.
	mkregion = Regions.newConsoleRegion Regions.Linear ""
	setregion r = Annex.changeState $ \s -> s { Annex.output = (Annex.output s) { consoleRegion = r } }
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
#endif

{- The progress region is displayed inline with the current console region. -}
#ifdef WITH_CONCURRENTOUTPUT
withProgressRegion :: (Regions.ConsoleRegion -> Annex a) -> Annex a
withProgressRegion a = do
	parent <- consoleRegion <$> Annex.getState Annex.output
	Regions.withConsoleRegion (maybe Regions.Linear Regions.InLine parent) a

instance Regions.LiftRegion Annex where
	liftRegion = liftIO . atomically
#endif

{- The concurrent-output library uses Text, which bypasses the normal use
 - of the fileSystemEncoding to roundtrip invalid characters, when in a
 - non-unicode locale. Work around that problem by avoiding using
 - concurrent output when not in a unicode locale. -}
concurrentOutputSupported :: IO Bool
#ifdef WITH_CONCURRENTOUTPUT
#ifndef mingw32_HOST_OS
concurrentOutputSupported = do
	enc <- getLocaleEncoding
	return ("UTF" `isInfixOf` textEncodingName enc)
#else
concurrentOutputSupported = return True -- Windows is always unicode
#endif
#else
concurrentOutputSupported = return False
#endif

concurrentOutputEnabled :: OutputType -> Bool
concurrentOutputEnabled (ConcurrentOutput _ b) = b
concurrentOutputEnabled _ = False

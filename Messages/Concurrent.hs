{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Messages.Concurrent where

import Types
import Types.Messages
import qualified Annex

#ifdef WITH_CONCURRENTOUTPUT
import Common
import qualified System.Console.Concurrent as Console
import qualified System.Console.Regions as Regions
import Control.Concurrent.STM
import qualified Data.Text as T
#ifndef mingw32_HOST_OS
import GHC.IO.Encoding
#endif
#endif

{- Outputs a message in a concurrency safe way.
 -
 - The message may be an error message, in which case it goes to stderr.
 -
 - When built without concurrent-output support, the fallback action is run
 - instead.
 -}
concurrentMessage :: MessageState -> Bool -> String -> Annex () -> Annex ()
#ifdef WITH_CONCURRENTOUTPUT
concurrentMessage s iserror msg fallback 
	| concurrentOutputEnabled s =
		go =<< consoleRegion <$> Annex.getState Annex.output
#else
concurrentMessage _s _iserror _msg fallback 
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
			Annex.changeState $ \st ->
				st { Annex.output = (Annex.output st) { consoleRegionErrFlag = True } }
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
inOwnConsoleRegion :: MessageState -> Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
inOwnConsoleRegion s a
	| concurrentOutputEnabled s = do
		r <- mkregion
		setregion (Just r)
		eret <- tryNonAsync a `onException` rmregion r
		case eret of
			Left e -> do
				-- Add error message to region before it closes.
				concurrentMessage s True (show e) noop
				rmregion r
				throwM e
			Right ret -> do
				rmregion r
				return ret
#else
inOwnConsoleRegion _s a
#endif
	| otherwise = a
#ifdef WITH_CONCURRENTOUTPUT
  where
	-- The region is allocated here, but not displayed until 
	-- a message is added to it. This avoids unnecessary screen
	-- updates when a region does not turn out to need to be used.
	mkregion = Regions.newConsoleRegion Regions.Linear ""
	setregion r = Annex.changeState $ \st -> st
		{ Annex.output = (Annex.output st) { consoleRegion = r } }
	rmregion r = do
		errflag <- consoleRegionErrFlag <$> Annex.getState Annex.output
		let h = if errflag then Console.StdErr else Console.StdOut
		Annex.changeState $ \st -> st
			{ Annex.output = (Annex.output st) { consoleRegionErrFlag = False } }
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

{- Hide any currently displayed console regions while running the action,
 - so that the action can use the console itself.
 - This needs a new enough version of concurrent-output; otherwise
 - the regions will not be hidden, but the action still runs, garbling the
 - display. -}
hideRegionsWhile :: Annex a -> Annex a
#ifdef WITH_CONCURRENTOUTPUT
#if MIN_VERSION_concurrent_output(1,9,0)
hideRegionsWhile a = bracketIO setup cleanup go
  where
	setup = Regions.waitDisplayChange $ swapTMVar Regions.regionList []
	cleanup = void . atomically . swapTMVar Regions.regionList
	go _ = do
		liftIO $ hFlush stdout
		a
#else
hideRegionsWhile = id
#endif
#else
hideRegionsWhile = id
#endif

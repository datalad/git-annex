{-# OPTIONS_GHC -fno-warn-orphans #-}

{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Messages.Concurrent where

import Common
import Annex
import Types.Messages

#ifdef WITH_CONCURRENTOUTPUT
import qualified System.Console.Concurrent as Console
import qualified System.Console.Regions as Regions
import Control.Concurrent.STM
import qualified Data.Text as T
#endif

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
		liftIO $ atomically $ do
			Regions.appendConsoleRegion r msg
			rl <- takeTMVar Regions.regionList
			putTMVar Regions.regionList
				(if r `elem` rl then rl else r:rl)

#else
concurrentMessage _ _ fallback = fallback
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
inOwnConsoleRegion a = do
	r <- mkregion
	setregion (Just r)
	eret <- tryNonAsync a `onException` rmregion r
	case eret of
		Left e -> do
			-- Add error message to region before it closes.
			concurrentMessage True (show e) noop
			rmregion r
			throwM e
		Right ret -> do
			rmregion r
			return ret
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
#else
inOwnConsoleRegion = id
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

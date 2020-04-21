{- git check-ignore interface, with handle automatically stored in
 - the Annex monad
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.CheckIgnore (
	checkIgnored,
	checkIgnoreStop,
	mkConcurrentCheckIgnoreHandle,
) where

import Annex.Common
import qualified Git.CheckIgnore as Git
import qualified Annex
import Utility.ResourcePool
import Types.Concurrency
import Annex.Concurrent.Utility

checkIgnored :: FilePath -> Annex Bool
checkIgnored file = withCheckIgnoreHandle $ \h ->
	liftIO $ Git.checkIgnored h file

withCheckIgnoreHandle :: (Git.CheckIgnoreHandle -> Annex a) -> Annex a
withCheckIgnoreHandle a =
	maybe mkpool go =<< Annex.getState Annex.checkignorehandle
  where
	go p = withResourcePool p start a
	start = inRepo Git.checkIgnoreStart
	mkpool = do
		-- This only runs in non-concurrent code paths;
		-- a concurrent pool is set up earlier when needed.
		p <- mkResourcePoolNonConcurrent start
		Annex.changeState $ \s -> s { Annex.checkignorehandle = Just p }
		go p

mkConcurrentCheckIgnoreHandle :: Concurrency -> Annex (ResourcePool Git.CheckIgnoreHandle)
mkConcurrentCheckIgnoreHandle c =
	Annex.getState Annex.checkignorehandle >>= \case
		Just p@(ResourcePool {}) -> return p
		_ -> mkResourcePool =<< liftIO (maxCheckIgnores c)

{- git check-ignore is typically CPU bound, and is not likely to be the main
 - bottleneck for any command. So limit to the number of CPU cores, maximum,
 - while respecting the -Jn value.
 -}
maxCheckIgnores :: Concurrency -> IO Int
maxCheckIgnores = concurrencyUpToCpus

checkIgnoreStop :: Annex ()
checkIgnoreStop = maybe noop stop =<< Annex.getState Annex.checkignorehandle
  where
	stop p = do
		liftIO $ freeResourcePool p Git.checkIgnoreStop
		Annex.changeState $ \s -> s { Annex.checkignorehandle = Nothing }

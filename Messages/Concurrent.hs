{- git-annex concurrent output
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Messages.Concurrent where

import Common.Annex
import Messages.Internal
import Types.Messages

#ifdef WITH_CONCURRENTOUTPUT
import qualified System.Console.Concurrent as Console
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

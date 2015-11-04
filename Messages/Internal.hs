{- git-annex output messages
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Messages.Internal where

import Common
import Types
import Types.Messages
import qualified Annex

#ifdef WITH_CONCURRENTOUTPUT
import System.Console.Concurrent
#endif

outputMessage :: IO () -> String -> Annex ()
outputMessage json s = withOutputType go
  where
	go NormalOutput = liftIO $
		flushed $ putStr s
	go QuietOutput = q
	go (ConcurrentOutput _) = liftIO $
#ifdef WITH_CONCURRENTOUTPUT
		outputConcurrent s
#else
		q
#endif
	go JSONOutput = liftIO $ flushed json

outputError :: String -> Annex ()
outputError s = withOutputType go
  where
	go (ConcurrentOutput _) = liftIO $
#ifdef WITH_CONCURRENTOUTPUT
		errorConcurrent s
#else
		q
#endif
	go _ = liftIO $ do
		hFlush stdout
		hPutStr stderr s
		hFlush stderr

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

withOutputType :: (OutputType -> Annex a) -> Annex a
withOutputType a = outputType <$> Annex.getState Annex.output >>= a

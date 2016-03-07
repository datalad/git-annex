{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages.Internal where

import Common
import Annex
import Types.Messages
import Messages.Concurrent

withOutputType :: (OutputType -> Annex a) -> Annex a
withOutputType a = outputType <$> Annex.getState Annex.output >>= a

outputMessage :: IO () -> String -> Annex ()
outputMessage json s = withOutputType go
  where
	go NormalOutput = liftIO $
		flushed $ putStr s
	go QuietOutput = q
	go o@(ConcurrentOutput {}) = concurrentMessage o False s q
	go JSONOutput = liftIO $ flushed json

outputError :: String -> Annex ()
outputError s = withOutputType go
  where
	go o@(ConcurrentOutput {}) = concurrentMessage o True s (go NormalOutput)
	go _ = liftIO $ do
		hFlush stdout
		hPutStr stderr s
		hFlush stderr

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

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

withMessageState :: (MessageState -> Annex a) -> Annex a
withMessageState a = Annex.getState Annex.output >>= a

outputMessage :: IO () -> String -> Annex ()
outputMessage json msg = withMessageState $ \s -> case outputType s of
	NormalOutput
		| concurrentOutputEnabled s -> concurrentMessage s False msg q
		| otherwise -> liftIO $ flushed $ putStr msg
	QuietOutput -> q
	JSONOutput -> liftIO $ flushed json

outputError :: String -> Annex ()
outputError msg = withMessageState $ \s ->
	if concurrentOutputEnabled s
		then concurrentMessage s True msg go
		else go
  where
	go = liftIO $ do
		hFlush stdout
		hPutStr stderr msg
		hFlush stderr

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

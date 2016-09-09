{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
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
outputMessage = outputMessage' False

outputMessageFinal :: IO () -> String -> Annex ()
outputMessageFinal = outputMessage' True

outputMessage' :: Bool -> IO () -> String -> Annex ()
outputMessage' endmessage json msg = withMessageState $ \s -> case outputType s of
	NormalOutput
		| concurrentOutputEnabled s -> concurrentMessage s False msg q
		| otherwise -> liftIO $ flushed $ putStr msg
	JSONOutput -> void $ outputJSON json endmessage s
	QuietOutput -> q

outputJSON :: IO () -> Bool -> MessageState -> Annex Bool
outputJSON json endmessage s = case outputType s of
	JSONOutput
		| concurrentOutputEnabled s -> do
			-- Buffer json fragments until end is reached.
			if endmessage
				then do
					Annex.changeState $ \st -> 
						st { Annex.output = s { jsonBuffer = [] } }
					liftIO $ flushed $ do
						sequence_ $ reverse $ jsonBuffer s
						json
				else Annex.changeState $ \st ->
				        st { Annex.output = s { jsonBuffer = json : jsonBuffer s } }
			return True
		| otherwise -> do
			liftIO $ flushed json
			return True
	_ -> return False

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

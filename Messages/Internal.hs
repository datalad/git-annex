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
import Messages.JSON

import qualified Data.ByteString.Lazy as B

withMessageState :: (MessageState -> Annex a) -> Annex a
withMessageState a = Annex.getState Annex.output >>= a

outputMessage :: JSONChunk -> String -> Annex ()
outputMessage = outputMessage' False

outputMessageFinal :: JSONChunk -> String -> Annex ()
outputMessageFinal = outputMessage' True

outputMessage' :: Bool -> JSONChunk -> String -> Annex ()
outputMessage' endmessage json msg = withMessageState $ \s -> case outputType s of
	NormalOutput
		| concurrentOutputEnabled s -> concurrentMessage s False msg q
		| otherwise -> liftIO $ flushed $ putStr msg
	JSONOutput _ -> void $ outputJSON json endmessage s
	QuietOutput -> q

outputJSON :: JSONChunk -> Bool -> MessageState -> Annex Bool
outputJSON json endmessage s = case outputType s of
	JSONOutput withprogress
		| withprogress || concurrentOutputEnabled s -> do
			-- Buffer json fragments until end is reached.
			if endmessage
				then do
					Annex.changeState $ \st -> 
						st { Annex.output = s { jsonBuffer = none } }
					liftIO $ flushed $ emit b
				else Annex.changeState $ \st ->
				        st { Annex.output = s { jsonBuffer = b } }
			return True
		| otherwise -> do
			liftIO $ flushed $ emit json
			return True
	_ -> return False
  where
	b = jsonBuffer s `B.append` json

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

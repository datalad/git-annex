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


withMessageState :: (MessageState -> Annex a) -> Annex a
withMessageState a = Annex.getState Annex.output >>= a

outputMessage :: JSONBuilder -> String -> Annex ()
outputMessage jsonbuilder msg = withMessageState $ \s -> case outputType s of
	NormalOutput
		| concurrentOutputEnabled s -> concurrentMessage s False msg q
		| otherwise -> liftIO $ flushed $ putStr msg
	JSONOutput _ -> void $ outputJSON jsonbuilder s
	QuietOutput -> q

-- Buffer changes to JSON until end is reached and then emit it.
outputJSON :: JSONBuilder -> MessageState -> Annex Bool
outputJSON jsonbuilder s = case outputType s of
	JSONOutput _
		| endjson -> do
			Annex.changeState $ \st -> 
				st { Annex.output = s { jsonBuffer = Nothing } }
			maybe noop (liftIO . flushed . emit) json
			return True
		| otherwise -> do
			Annex.changeState $ \st ->
			        st { Annex.output = s { jsonBuffer = json } }
			return True
	_ -> return False
  where
	(json, endjson) = case jsonbuilder i of
		Nothing -> (jsonBuffer s, False)
		(Just (j, e)) -> (Just j, e)
	i = case jsonBuffer s of
		Nothing -> Nothing
		Just b -> Just (b, False)

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

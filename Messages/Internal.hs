{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Messages.Internal where

import Common
import Annex
import Types.Messages
import Messages.Concurrent
import qualified Messages.JSON as JSON
import Messages.JSON (JSONBuilder)

import qualified Data.ByteString as S

withMessageState :: (MessageState -> Annex a) -> Annex a
withMessageState a = Annex.getState Annex.output >>= a

outputMessage :: JSONBuilder -> S.ByteString -> Annex ()
outputMessage = outputMessage' bufferJSON

outputMessage' :: (JSONBuilder -> MessageState -> Annex Bool) -> JSONBuilder -> S.ByteString -> Annex ()
outputMessage' jsonoutputter jsonbuilder msg = withMessageState $ \s -> case outputType s of
	NormalOutput
		| concurrentOutputEnabled s -> concurrentMessage s False (decodeBS msg) q
		| otherwise -> liftIO $ flushed $ S.putStr msg
	JSONOutput _ -> void $ jsonoutputter jsonbuilder s
	QuietOutput -> q
	SerializedOutput -> do
		liftIO $ outputSerialized $ OutputMessage msg
		void $ jsonoutputter jsonbuilder s

-- Buffer changes to JSON until end is reached and then emit it.
bufferJSON :: JSONBuilder -> MessageState -> Annex Bool
bufferJSON jsonbuilder s = case outputType s of
	JSONOutput _ -> go (flushed . JSON.emit)
	SerializedOutput -> go (outputSerialized . JSONObject . JSON.encode)
	_ -> return False
  where
	go emitter
		| endjson = do
			Annex.changeState $ \st -> 
				st { Annex.output = s { jsonBuffer = Nothing } }
			maybe noop (liftIO . emitter . JSON.finalize) json
			return True
		| otherwise = do
			Annex.changeState $ \st ->
			        st { Annex.output = s { jsonBuffer = json } }
			return True
	
	(json, endjson) = case jsonbuilder i of
		Nothing -> (jsonBuffer s, False)
		(Just (j, e)) -> (Just j, e)
	
	i = case jsonBuffer s of
		Nothing -> Nothing
		Just b -> Just (b, False)

-- Immediately output JSON.
outputJSON :: JSONBuilder -> MessageState -> Annex Bool
outputJSON jsonbuilder s = case outputType s of
	JSONOutput _ -> go (flushed . JSON.emit)
	SerializedOutput -> go (outputSerialized . JSONObject . JSON.encode)
	_ -> return False
  where
	go emitter = do
		maybe noop (liftIO . emitter)
			(fst <$> jsonbuilder Nothing)
		return True

outputError :: String -> Annex ()
outputError msg = withMessageState $ \s -> case (outputType s, jsonBuffer s) of
        (JSONOutput jsonoptions, Just jb) | jsonErrorMessages jsonoptions ->
		let jb' = Just (JSON.addErrorMessage (lines msg) jb)
		in Annex.changeState $ \st ->
			st { Annex.output = s { jsonBuffer = jb' } }
	(SerializedOutput, _) -> 
		liftIO $ outputSerialized $ OutputError msg
	_
		| concurrentOutputEnabled s -> concurrentMessage s True msg go
		| otherwise -> go
  where
	go = liftIO $ do
		hFlush stdout
		hPutStr stderr msg
		hFlush stderr

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

outputSerialized :: SerializedOutput -> IO ()
outputSerialized = print

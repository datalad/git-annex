{- git-annex output messages, including concurrent output to display regions
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
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
import Git.Filename
import Types.GitConfig

import qualified Data.ByteString as S

withMessageState :: (MessageState -> Annex a) -> Annex a
withMessageState a = Annex.getState Annex.output >>= a

outputMessage :: JSONBuilder -> (S.ByteString -> S.ByteString) -> StringContainingQuotedPath -> Annex ()
outputMessage = outputMessage' bufferJSON

outputMessage' :: (JSONBuilder -> MessageState -> Annex Bool) -> JSONBuilder -> (S.ByteString -> S.ByteString) -> StringContainingQuotedPath -> Annex ()
outputMessage' jsonoutputter jsonbuilder consolewhitespacef msg = withMessageState $ \s -> case outputType s of
	NormalOutput
		| concurrentOutputEnabled s -> do
			qp <- coreQuotePath <$> Annex.getGitConfig
			liftIO $ clearProgressMeter s
			concurrentMessage s False (decodeBS (consolewhitespacef (quote qp msg))) q
		| otherwise -> do
			qp <- coreQuotePath <$> Annex.getGitConfig
			liftIO $ clearProgressMeter s
			liftIO $ flushed $ S.putStr (consolewhitespacef (quote qp msg))
	JSONOutput _ -> void $ jsonoutputter jsonbuilder s
	QuietOutput -> q
	SerializedOutput h _ -> do
		qp <- coreQuotePath <$> Annex.getGitConfig
		liftIO $ outputSerialized h $ OutputMessage $ consolewhitespacef $ quote qp msg
		void $ jsonoutputter jsonbuilder s

-- Buffer changes to JSON until end is reached and then emit it.
bufferJSON :: JSONBuilder -> MessageState -> Annex Bool
bufferJSON jsonbuilder s = case outputType s of
	JSONOutput _ -> go (flushed . JSON.emit)
	SerializedOutput h _ -> go (outputSerialized h . JSONObject . JSON.encode)
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
	SerializedOutput h _ -> go (outputSerialized h . JSONObject . JSON.encode)
	_ -> return False
  where
	go emitter = do
		maybe noop (liftIO . emitter)
			(fst <$> jsonbuilder Nothing)
		return True

outputError :: (S.ByteString -> S.ByteString) -> StringContainingQuotedPath -> Annex ()
outputError consolewhitespacef msg = withMessageState $ \s -> case (outputType s, jsonBuffer s) of
        (JSONOutput jsonoptions, Just jb) | jsonErrorMessages jsonoptions ->
		let jb' = Just (JSON.addErrorMessage (lines (decodeBS (noquote msg))) jb)
		in Annex.changeState $ \st ->
			st { Annex.output = s { jsonBuffer = jb' } }
	(SerializedOutput h _, _) -> do
		qp <- coreQuotePath <$> Annex.getGitConfig
		liftIO $ outputSerialized h $ OutputError $ decodeBS $
			consolewhitespacef $ quote qp msg
	_
		| concurrentOutputEnabled s -> do
			qp <- coreQuotePath <$> Annex.getGitConfig
			concurrentMessage s True (decodeBS $ consolewhitespacef $ quote qp msg) go
		| otherwise -> go
  where
	go = do
		qp <- coreQuotePath <$> Annex.getGitConfig
		liftIO $ hFlush stdout
		liftIO $ S.hPutStr stderr (consolewhitespacef $ quote qp msg)
		liftIO $ hFlush stderr

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

outputSerialized :: (SerializedOutput -> IO ()) -> SerializedOutput -> IO ()
outputSerialized = id

-- | Wait for the specified response.
waitOutputSerializedResponse :: (IO (Maybe SerializedOutputResponse)) -> SerializedOutputResponse -> IO ()
waitOutputSerializedResponse getr r = tryIO getr >>= \case
	Right (Just r') | r' == r -> return ()
	v -> giveup $ "serialized output protocol error; expected " ++ show r ++ " got " ++ show v

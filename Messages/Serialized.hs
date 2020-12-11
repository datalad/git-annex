{- serialized output
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Messages.Serialized (
	relaySerializedOutput,
	outputSerialized,
	waitOutputSerializedResponse,
) where

import Common
import Annex
import Types.Messages
import Messages
import Messages.Internal
import Messages.Progress
import qualified Messages.JSON as JSON
import Utility.Metered (BytesProcessed, setMeterTotalSize)

import Control.Monad.IO.Class (MonadIO)

-- | Relay serialized output from a child process to the console.
relaySerializedOutput
	:: (Monad m, MonadIO m, MonadMask m)
	=> m (Either SerializedOutput r)
	-- ^ Get next serialized output, or final value to return.
	-> (SerializedOutputResponse -> m ())
	-- ^ Send response to child process.
	-> (Maybe BytesProcessed -> m ())
	-- ^ When a progress meter is running, is updated with
	-- progress meter values sent by the process.
	-- When a progress meter is stopped, Nothing is sent.
	-> (forall a. Annex a -> m a)
	-- ^ Run an annex action in the monad. Will not be used with
	-- actions that block for a long time.
	-> m r
relaySerializedOutput getso sendsor meterreport runannex = go Nothing
  where
	go st = loop st >>= \case
		Right r -> return r
		Left st' -> go st'
	
	loop st = getso >>= \case
		Right r -> return (Right r)
		Left (OutputMessage msg) -> do
			runannex $ outputMessage'
				(\_ _ -> return False)
				id
				msg
			loop st
		Left (OutputError msg) -> do
			runannex $ outputError msg
			loop st		
		Left (JSONObject b) -> do
			runannex $ withMessageState $ \s -> case outputType s of
				JSONOutput _ -> liftIO $ flushed $ JSON.emit' b
				SerializedOutput h _ -> liftIO $
					outputSerialized h $ JSONObject b
				_ -> q
			loop st
		Left (BeginProgressMeter sz) -> do
			ost <- runannex (Annex.getState Annex.output)
			-- Display a progress meter while running, until
			-- the meter ends or a final value is returned.
			metered' ost Nothing sz (runannex showOutput) 
				(\meter meterupdate -> loop (Just (meter, meterupdate)))
				>>= \case
					Right r -> return (Right r)
					-- Continue processing serialized
					-- output after the progress meter
					-- is done.
					Left _st' -> loop Nothing
		Left EndProgressMeter -> do
			meterreport Nothing
			return (Left st)
		Left (UpdateProgressMeter n) -> do
			case st of
				Just (_, meterupdate) -> do
					meterreport (Just n)
					liftIO $ meterupdate n
				Nothing -> noop
			loop st
		Left (UpdateProgressMeterTotalSize sz) -> do
			case st of
				Just (meter, _) -> liftIO $
					setMeterTotalSize meter sz
				Nothing -> noop
			loop st
		Left BeginPrompt -> do
			prompter <- runannex mkPrompter
			v <- prompter $ do
				sendsor ReadyPrompt
				-- Continue processing serialized output
				-- until EndPrompt or a final value is
				-- returned. (EndPrompt is all that
				-- ought to be sent while in a prompt
				-- really, but if something else did get
				-- sent, display it just in case.)
				loop st
			case v of
				Right r -> return (Right r)
				Left st' -> loop st'
		Left EndPrompt -> return (Left st)

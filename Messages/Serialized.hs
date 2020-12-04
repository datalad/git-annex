{- serialized output
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Messages.Serialized (outputSerialized, relaySerializedOutput) where

import Common
import Annex
import Types.Messages
import Messages
import Messages.Internal
import Messages.Progress
import qualified Messages.JSON as JSON

import Control.Monad.IO.Class (MonadIO)

relaySerializedOutput
	:: (Monad m, MonadIO m, MonadMask m)
	=> m (Either SerializedOutput r)
	-- ^ Get next serialized output, or final value to return.
	-> (forall a. Annex a -> m a)
	-- ^ Run an annex action in the monad. Will not be used with
	-- actions that block for a long time.
	-> m r
relaySerializedOutput getso runannex = go Nothing
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
				SerializedOutput h -> liftIO $
					outputSerialized h $ JSONObject b
				_ -> q
			loop st
		Left (StartProgressMeter sz) -> do
			ost <- runannex (Annex.getState Annex.output)
			-- Display a progress meter while running, until
			-- the meter ends or a final value is returned.
			metered' ost Nothing sz (runannex showOutput) 
				(\_meter meterupdate -> loop (Just meterupdate))
				>>= \case
					Right r -> return (Right r)
					-- Continue processing serialized
					-- output after the progress meter
					-- is done.
					Left _st' -> loop Nothing
		Left EndProgressMeter -> return (Left st)
		Left (UpdateProgressMeter n) -> do
			case st of
				Just meterupdate -> liftIO $ meterupdate n
				Nothing -> noop
			loop st

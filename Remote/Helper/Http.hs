{- helpers for remotes using http
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Http where

import Common.Annex
import Types.StoreRetrieve
import Utility.Metered
import Remote.Helper.Special
import Network.HTTP.Client (RequestBody(..), Response, responseStatus, responseBody, BodyReader)
import Network.HTTP.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Concurrent

-- A storer that expects to be provided with a http RequestBody containing
-- the content to store.
--
-- Implemented as a fileStorer, so that the content can be streamed
-- from the file in constant space.
httpStorer :: (Key -> RequestBody -> Annex Bool) -> Storer
httpStorer a = fileStorer $ \k f m -> a k =<< liftIO (httpBodyStorer f m)

-- Reads the file and generates a streaming request body, that will update
-- the meter as it's sent.
httpBodyStorer :: FilePath -> MeterUpdate -> IO RequestBody
httpBodyStorer src m = do
	size <- fromIntegral . fileSize <$> getFileStatus src :: IO Integer
	let streamer sink = withMeteredFile src m $ \b -> do
		mvar <- newMVar $ L.toChunks b
		let getnextchunk = modifyMVar mvar $ pure . pop
		sink getnextchunk
	return $ RequestBodyStream (fromInteger size) streamer
  where
	pop [] = ([], S.empty)
	pop (c:cs) = (cs, c)

-- Reads the http body and stores it to the specified file, updating the
-- meter as it goes.
httpBodyRetriever :: FilePath -> MeterUpdate -> Response BodyReader -> IO ()
httpBodyRetriever dest meterupdate resp
	| responseStatus resp /= ok200 = error $ show $ responseStatus resp
	| otherwise = bracket (openBinaryFile dest WriteMode) hClose (go zeroBytesProcessed)
  where
	reader = responseBody resp
	go sofar h = do
		b <- reader
		if S.null b
			then return ()
			else do
				let sofar' = addBytesProcessed sofar $ S.length b
				S.hPut h b
				meterupdate sofar'
				go sofar' h

{- helpers for remotes using http
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Remote.Helper.Http where

import Annex.Common
import Types.StoreRetrieve
import Remote.Helper.Special
import Utility.Metered
import Utility.Hash (IncrementalVerifier(..))
import qualified Utility.FileIO as F

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Concurrent
import Network.HTTP.Client (RequestBody(..), Response, responseStatus, responseBody, BodyReader, NeedsPopper)
import Network.HTTP.Types

-- A storer that expects to be provided with a http RequestBody containing
-- the content to store.
--
-- Implemented as a fileStorer, so that the content can be streamed
-- from the file in constant space.
httpStorer :: (Key -> RequestBody -> Annex ()) -> Storer
httpStorer a = fileStorer $ \k f m -> a k =<< liftIO (httpBodyStorer f m)

-- Reads the file and generates a streaming request body, that will update
-- the meter as it's sent.
httpBodyStorer :: OsPath -> MeterUpdate -> IO RequestBody
httpBodyStorer src m = do
	size <- getFileSize src
	let streamer sink = withMeteredFile src m $ \b -> byteStringPopper b sink
	return $ RequestBodyStream (fromInteger size) streamer

-- Like httpBodyStorer, but generates a chunked request body.
httpBodyStorerChunked :: OsPath -> MeterUpdate -> RequestBody
httpBodyStorerChunked src m =
	let streamer sink = withMeteredFile src m $ \b -> byteStringPopper b sink
	in RequestBodyStreamChunked streamer

byteStringPopper :: L.ByteString -> NeedsPopper () -> IO ()
byteStringPopper b sink = do
	mvar <- newMVar $ L.toChunks b
	let getnextchunk = modifyMVar mvar $ \v ->
		case v of
			[] -> return ([], S.empty)
			(c:cs) -> return (cs, c)
	sink getnextchunk

{- Makes a Popper that streams a given number of chunks of a given
 - size from the handle, updating the meter as the chunks are read. -}
handlePopper :: Integer -> Int -> MeterUpdate -> Handle -> NeedsPopper () -> IO ()
handlePopper numchunks chunksize meterupdate h sink = do
	mvar <- newMVar zeroBytesProcessed
	let getnextchunk = do
		sent <- takeMVar mvar
		if sent >= target
			then do
				putMVar mvar sent
				return S.empty
			else do
				b <- S.hGet h chunksize
				let !sent' = addBytesProcessed sent chunksize
				putMVar mvar sent'
                                meterupdate sent'
				return b
	sink getnextchunk
  where
	target = toBytesProcessed (numchunks * fromIntegral chunksize)

-- Reads the http body and stores it to the specified file, updating the
-- meter and incremental verifier as it goes.
httpBodyRetriever :: OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> Response BodyReader -> IO ()
httpBodyRetriever dest meterupdate iv resp
	| responseStatus resp /= ok200 = giveup $ show $ responseStatus resp
	| otherwise = bracket (F.openBinaryFile dest WriteMode) hClose (go zeroBytesProcessed)
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
				maybe noop (flip updateIncrementalVerifier b) iv
				go sofar' h

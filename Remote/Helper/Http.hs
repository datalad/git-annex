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
import Network.HTTP.Client (RequestBody(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Concurrent

-- A storer that expects to be provided with a http RequestBody containing
-- the content to store.
--
-- Implemented as a fileStorer, so that the content can be streamed
-- from the file in constant space.
httpStorer :: (Key -> RequestBody -> Annex Bool) -> Storer
httpStorer a = fileStorer $ \k f m -> do
	size <- liftIO $ (fromIntegral . fileSize <$> getFileStatus f :: IO Integer)
	let streamer sink = withMeteredFile f m $ \b -> do
		mvar <- newMVar $ L.toChunks b
		let getnextchunk = modifyMVar mvar $ pure . pop
		sink getnextchunk
	let body = RequestBodyStream (fromInteger size) streamer
	a k body
  where
	pop [] = ([], S.empty)
	pop (c:cs) = (cs, c)

--httpRetriever :: (Key -> Annex Response) -> Retriever
--httpRetriever a = byteRetriever $ \k sink 

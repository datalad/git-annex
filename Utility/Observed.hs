module Utility.Observed where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import System.IO
import System.IO.Unsafe
import Foreign.Storable (Storable(sizeOf))

{- This is like L.hGetContents, but after each chunk is read, an action
 - is run to observe the size of the chunk.
 -
 - Note that the observer is run in unsafeInterleaveIO, which means that
 - it can be run at any time. It's even possible for observers to run out
 - of order, as different parts of the ByteString are consumed.
 -
 - All the usual caveats about using unsafeInterleaveIO apply to the observers,
 - so use caution.
 -}
hGetContentsObserved :: Handle -> (Int -> IO ()) -> IO L.ByteString
hGetContentsObserved h observe = lazyRead
  where
	lazyRead = unsafeInterleaveIO loop

	loop = do
		c <- S.hGetSome h defaultChunkSize
		if S.null c
			then do
				hClose h
				return $ L.empty
			else do
				observe $ S.length c
				{- unsafeInterleaveIO causes this to be
				 - deferred until the data is read from the
				 - ByteString. -}
				cs <- lazyRead
				return $ L.append (L.fromChunks [c]) cs

{- Same default chunk size Lazy ByteStrings use. -}
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
  where
	k = 1024
	chunkOverhead = 2 * sizeOf (undefined :: Int) -- GHC specific

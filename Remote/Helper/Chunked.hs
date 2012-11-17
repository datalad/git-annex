{- git-annex chunked remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked where

import Common.Annex
import Utility.DataUnits
import Types.Remote

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Int
import qualified Control.Exception as E

type ChunkSize = Maybe Int64

{- Gets a remote's configured chunk size. -}
chunkSize :: Maybe RemoteConfig -> ChunkSize
chunkSize Nothing = Nothing
chunkSize (Just m) =
	case M.lookup "chunksize" m of
		Nothing -> Nothing
		Just v -> case readSize dataUnits v of
			Nothing -> error "bad chunksize"
			Just size
				| size <= 0 -> error "bad chunksize"
				| otherwise -> Just $ fromInteger size

{- This is an extension that's added to the usual file (or whatever)
 - where the remote stores a key. -}
type ChunkExt = String

{- A record of the number of chunks used.
 -
 - While this can be guessed at based on the size of the key, encryption
 - makes that larger. Also, using this helps deal with changes to chunksize
 - over the life of a remote.
 -}
chunkCount :: ChunkExt
chunkCount = ".chunkcount"

{- Parses the String from the chunkCount file, and returns the files that
 - are used to store the chunks. -}
listChunks :: FilePath -> String -> [FilePath]
listChunks basedest chunkcount = take count $ map (basedest ++) chunkStream
  where
	count = fromMaybe 0 $ readish chunkcount

{- An infinite stream of extensions to use for chunks. -}
chunkStream :: [ChunkExt]
chunkStream = map (\n -> ".chunk" ++ show n) [1 :: Integer ..]

{- Given the base destination to use to store a value,
 - generates a stream of temporary destinations (just one when not chunking)
 - and passes it to an action, which should chunk and store the data,
 - and return the destinations it stored to, or [] on error.
 -
 - Then calles the finalizer to rename the temporary destinations into
 - their final places (and do any other cleanup), and writes the chunk count
 - (if chunking)
 -}
storeChunks :: FilePath -> ChunkSize -> ([FilePath] -> IO [FilePath]) -> (FilePath -> String -> IO ()) -> (FilePath -> FilePath -> IO ()) -> IO Bool
storeChunks basedest chunksize storer recorder finalizer =
	either (const $ return False) return
		=<< (E.try go :: IO (Either E.SomeException Bool))
  where
	go = do
		stored <- storer tmpdests
		forM_ stored $ \d -> do
			let dest = detmpprefix d
			finalizer d dest
		when (chunksize /= Nothing) $ do
			let chunkcount = basedest ++ chunkCount
			recorder chunkcount (show $ length stored)
		return (not $ null stored)

	tmpprefix = ".tmp"
	detmpprefix f = take (length f - tmpprefixlen) f
	tmpprefixlen = length tmpprefix
	tmpdests
		| chunksize == Nothing = [basedest ++ tmpprefix]
		| otherwise = map (++ tmpprefix) $ map (basedest ++) chunkStream

{- Given a list of destinations to use, chunks the data according to the
 - ChunkSize, and runs the storer action to store each chunk. Returns
 - the destinations where data was stored, or [] on error.
 -
 - This buffers each chunk in memory.
 - More optimal versions of this can be written, that rely
 - on L.toChunks to split the lazy bytestring into chunks (typically
 - smaller than the ChunkSize), and eg, write those chunks to a Handle.
 - But this is the best that can be done with the storer interface that
 - writes a whole L.ByteString at a time.
 -}
storeChunked :: ChunkSize -> [FilePath] -> (FilePath -> L.ByteString -> IO ()) -> L.ByteString -> IO [FilePath]
storeChunked chunksize dests storer content =
	either (const $ return []) return
		=<< (E.try (go chunksize dests) :: IO (Either E.SomeException [FilePath]))
  where
	go _ [] = return [] -- no dests!?

	go Nothing (d:_) = do
		storer d content
		return [d]

	go (Just sz) _
		-- always write a chunk, even if the data is 0 bytes
		| L.null content = go Nothing dests
		| otherwise = storechunks sz [] dests content
		
	storechunks _ _ [] _ = return [] -- ran out of dests
	storechunks sz useddests (d:ds) b
		| L.null b = return $ reverse useddests
		| otherwise = do
			let (chunk, b') = L.splitAt sz b
			storer d chunk
			storechunks sz (d:useddests) ds b'

{- Write a L.ByteString to a file, updating a progress meter
 - after each chunk of the L.ByteString, typically every 64 kb or so. -}
meteredWriteFile :: MeterUpdate -> FilePath -> L.ByteString -> IO ()
meteredWriteFile meterupdate dest b =
	meteredWriteFileChunks meterupdate dest (L.toChunks b) feeder
  where
	feeder chunks = return ([], chunks)

{- Writes a series of S.ByteString chunks to a file, updating a progress
 - meter after each chunk. The feeder is called to get more chunks. -}
meteredWriteFileChunks :: MeterUpdate -> FilePath -> s -> (s -> IO (s, [S.ByteString])) -> IO ()
meteredWriteFileChunks meterupdate dest startstate feeder =
	E.bracket (openFile dest WriteMode) hClose (feed startstate [])
  where
	feed state [] h = do
		(state', cs) <- feeder state
		unless (null cs) $
			feed state' cs h
	feed state (c:cs) h = do
		S.hPut h c
		meterupdate $ toInteger $ S.length c
		feed state cs h

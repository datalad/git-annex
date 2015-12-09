{- Metered IO and actions
 -
 - Copyright 2012-2105 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE TypeSynonymInstances #-}

module Utility.Metered where

import Common

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import System.IO.Unsafe
import Foreign.Storable (Storable(sizeOf))
import System.Posix.Types
import Data.Int
import Data.Bits.Utils
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO)

{- An action that can be run repeatedly, updating it on the bytes processed.
 -
 - Note that each call receives the total number of bytes processed, so
 - far, *not* an incremental amount since the last call. -}
type MeterUpdate = (BytesProcessed -> IO ())

nullMeterUpdate :: MeterUpdate
nullMeterUpdate _ = return ()

combineMeterUpdate :: MeterUpdate -> MeterUpdate -> MeterUpdate
combineMeterUpdate a b = \n -> a n >> b n

{- Total number of bytes processed so far. -}
newtype BytesProcessed = BytesProcessed Integer
	deriving (Eq, Ord, Show)

class AsBytesProcessed a where
	toBytesProcessed :: a -> BytesProcessed
	fromBytesProcessed :: BytesProcessed -> a

instance AsBytesProcessed BytesProcessed where
	toBytesProcessed = id
	fromBytesProcessed = id

instance AsBytesProcessed Integer where
	toBytesProcessed i = BytesProcessed i
	fromBytesProcessed (BytesProcessed i) = i

instance AsBytesProcessed Int where
	toBytesProcessed i = BytesProcessed $ toInteger i
	fromBytesProcessed (BytesProcessed i) = fromInteger i

instance AsBytesProcessed Int64 where
	toBytesProcessed i = BytesProcessed $ toInteger i
	fromBytesProcessed (BytesProcessed i) = fromInteger i

instance AsBytesProcessed FileOffset where
	toBytesProcessed sz = BytesProcessed $ toInteger sz
	fromBytesProcessed (BytesProcessed sz) = fromInteger sz

addBytesProcessed :: AsBytesProcessed v => BytesProcessed -> v -> BytesProcessed
addBytesProcessed (BytesProcessed i) v = 
	let (BytesProcessed n) = toBytesProcessed v
	in BytesProcessed $! i + n

zeroBytesProcessed :: BytesProcessed
zeroBytesProcessed = BytesProcessed 0

{- Sends the content of a file to an action, updating the meter as it's
 - consumed. -}
withMeteredFile :: FilePath -> MeterUpdate -> (L.ByteString -> IO a) -> IO a
withMeteredFile f meterupdate a = withBinaryFile f ReadMode $ \h ->
	hGetContentsMetered h meterupdate >>= a

{- Sends the content of a file to a Handle, updating the meter as it's
 - written. -}
streamMeteredFile :: FilePath -> MeterUpdate -> Handle -> IO ()
streamMeteredFile f meterupdate h = withMeteredFile f meterupdate $ L.hPut h

{- Writes a ByteString to a Handle, updating a meter as it's written. -}
meteredWrite :: MeterUpdate -> Handle -> L.ByteString -> IO ()
meteredWrite meterupdate h = go zeroBytesProcessed . L.toChunks 
  where
	go _ [] = return ()
	go sofar (c:cs) = do
		S.hPut h c
		let sofar' = addBytesProcessed sofar $ S.length c
		meterupdate sofar'
		go sofar' cs

meteredWriteFile :: MeterUpdate -> FilePath -> L.ByteString -> IO ()
meteredWriteFile meterupdate f b = withBinaryFile f WriteMode $ \h ->
	meteredWrite meterupdate h b

{- Applies an offset to a MeterUpdate. This can be useful when
 - performing a sequence of actions, such as multiple meteredWriteFiles,
 - that all update a common meter progressively. Or when resuming.
 -}
offsetMeterUpdate :: MeterUpdate -> BytesProcessed -> MeterUpdate
offsetMeterUpdate base offset = \n -> base (offset `addBytesProcessed` n)

{- This is like L.hGetContents, but after each chunk is read, a meter
 - is updated based on the size of the chunk.
 -
 - All the usual caveats about using unsafeInterleaveIO apply to the
 - meter updates, so use caution.
 -}
hGetContentsMetered :: Handle -> MeterUpdate -> IO L.ByteString
hGetContentsMetered h = hGetUntilMetered h (const True)

{- Reads from the Handle, updating the meter after each chunk.
 -
 - Note that the meter update is run in unsafeInterleaveIO, which means that
 - it can be run at any time. It's even possible for updates to run out
 - of order, as different parts of the ByteString are consumed.
 -
 - Stops at EOF, or when keepgoing evaluates to False.
 - Closes the Handle at EOF, but otherwise leaves it open.
 -}
hGetUntilMetered :: Handle -> (Integer -> Bool) -> MeterUpdate -> IO L.ByteString
hGetUntilMetered h keepgoing meterupdate = lazyRead zeroBytesProcessed
  where
	lazyRead sofar = unsafeInterleaveIO $ loop sofar

	loop sofar = do
		c <- S.hGet h defaultChunkSize
		if S.null c
			then do
				hClose h
				return $ L.empty
			else do
				let sofar' = addBytesProcessed sofar (S.length c)
				meterupdate sofar'
				if keepgoing (fromBytesProcessed sofar')
					then do
						{- unsafeInterleaveIO causes this to be
						 - deferred until the data is read from the
						 - ByteString. -}
						cs <- lazyRead sofar'
						return $ L.append (L.fromChunks [c]) cs
					else return $ L.fromChunks [c]

{- Same default chunk size Lazy ByteStrings use. -}
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
  where
	k = 1024
	chunkOverhead = 2 * sizeOf (1 :: Int) -- GHC specific

{- Runs an action, watching a file as it grows and updating the meter. -}
watchFileSize :: (MonadIO m, MonadMask m) => FilePath -> MeterUpdate -> m a -> m a
watchFileSize f p a = bracket 
	(liftIO $ forkIO $ watcher zeroBytesProcessed)
	(liftIO . void . tryIO . killThread)
	(const a)
  where
	watcher oldsz = do
		v <- catchMaybeIO $ toBytesProcessed <$> getFileSize f
		newsz <- case v of
			Just sz | sz /= oldsz -> do
				p sz
				return sz
			_ -> return oldsz
		threadDelay 500000 -- 0.5 seconds
		watcher newsz

data OutputHandler = OutputHandler
	{ quietMode :: Bool
	, stderrHandler :: String -> IO ()
	}

{- Parses the String looking for a command's progress output, and returns
 - Maybe the number of bytes done so far, and any any remainder of the
 - string that could be an incomplete progress output. That remainder
 - should be prepended to future output, and fed back in. This interface
 - allows the command's output to be read in any desired size chunk, or
 - even one character at a time.
 -}
type ProgressParser = String -> (Maybe BytesProcessed, String)

{- Runs a command and runs a ProgressParser on its output, in order
 - to update a meter.
 -}
commandMeter :: ProgressParser -> OutputHandler -> MeterUpdate -> FilePath -> [CommandParam] -> IO Bool
commandMeter progressparser oh meterupdate cmd params = 
	outputFilter cmd params Nothing
		(feedprogress zeroBytesProcessed [])
		handlestderr
  where
	feedprogress prev buf h = do
		b <- S.hGetSome h 80
		if S.null b
			then return ()
			else do
				unless (quietMode oh) $ do
					S.hPut stdout b
					hFlush stdout
				let s = w82s (S.unpack b)
				let (mbytes, buf') = progressparser (buf++s)
				case mbytes of
					Nothing -> feedprogress prev buf' h
					(Just bytes) -> do
						when (bytes /= prev) $
							meterupdate bytes
						feedprogress bytes buf' h

	handlestderr h = unlessM (hIsEOF h) $ do
		stderrHandler oh =<< hGetLine h
		handlestderr h

{- Runs a command, that may display one or more progress meters on
 - either stdout or stderr, and prevents the meters from being displayed.
 -
 - The other command output is handled as configured by the OutputHandler.
 -}
demeterCommand :: OutputHandler -> FilePath -> [CommandParam] -> IO Bool
demeterCommand oh cmd params = demeterCommandEnv oh cmd params Nothing

demeterCommandEnv :: OutputHandler -> FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
demeterCommandEnv oh cmd params environ = outputFilter cmd params environ
	(\outh -> avoidProgress True outh stdouthandler)
	(\errh -> avoidProgress True errh $ stderrHandler oh)
  where
	stdouthandler l = 
		unless (quietMode oh) $
			putStrLn l

{- To suppress progress output, while displaying other messages,
 - filter out lines that contain \r (typically used to reset to the
 - beginning of the line when updating a progress display).
 -}
avoidProgress :: Bool -> Handle -> (String -> IO ()) -> IO ()
avoidProgress doavoid h emitter = unlessM (hIsEOF h) $ do
	s <- hGetLine h
	unless (doavoid && '\r' `elem` s) $
		emitter s
	avoidProgress doavoid h emitter

outputFilter
	:: FilePath
	-> [CommandParam]
	-> Maybe [(String, String)]
	-> (Handle -> IO ())
	-> (Handle -> IO ())
	-> IO Bool
outputFilter cmd params environ outfilter errfilter = catchBoolIO $ do
	(_, Just outh, Just errh, pid) <- createProcess p
		{ std_out = CreatePipe
		, std_err = CreatePipe
		}
	void $ async $ tryIO (outfilter outh) >> hClose outh
	void $ async $ tryIO (errfilter errh) >> hClose errh
	ret <- checkSuccessProcess pid
	return ret
  where
	p = (proc cmd (toCommand params))
		{ env = environ }

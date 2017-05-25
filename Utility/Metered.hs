{- Metered IO and actions
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}

module Utility.Metered where

import Common
import Utility.FileSystemEncoding
import Utility.Percentage
import Utility.DataUnits
import Utility.HumanTime

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import System.IO.Unsafe
import Foreign.Storable (Storable(sizeOf))
import System.Posix.Types
import Data.Int
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock
import Data.Time.Clock.POSIX

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
meteredWrite meterupdate h = void . meteredWrite' meterupdate h

meteredWrite' :: MeterUpdate -> Handle -> L.ByteString -> IO BytesProcessed
meteredWrite' meterupdate h = go zeroBytesProcessed . L.toChunks 
  where
	go sofar [] = return sofar
	go sofar (c:cs) = do
		S.hPut h c
		let !sofar' = addBytesProcessed sofar $ S.length c
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
hGetContentsMetered h = hGetMetered h Nothing

{- Reads from the Handle, updating the meter after each chunk is read.
 -
 - Stops at EOF, or when the requested number of bytes have been read.
 - Closes the Handle at EOF, but otherwise leaves it open.
 -
 - Note that the meter update is run in unsafeInterleaveIO, which means that
 - it can be run at any time. It's even possible for updates to run out
 - of order, as different parts of the ByteString are consumed.
 -}
hGetMetered :: Handle -> Maybe Integer -> MeterUpdate -> IO L.ByteString
hGetMetered h wantsize meterupdate = lazyRead zeroBytesProcessed
  where
	lazyRead sofar = unsafeInterleaveIO $ loop sofar

	loop sofar = do
		c <- S.hGet h (nextchunksize (fromBytesProcessed sofar))
		if S.null c
			then do
				hClose h
				return $ L.empty
			else do
				let !sofar' = addBytesProcessed sofar (S.length c)
				meterupdate sofar'
				if keepgoing (fromBytesProcessed sofar')
					then do
						{- unsafeInterleaveIO causes this to be
						 - deferred until the data is read from the
						 - ByteString. -}
						cs <- lazyRead sofar'
						return $ L.append (L.fromChunks [c]) cs
					else return $ L.fromChunks [c]
	
	keepgoing n = case wantsize of
		Nothing -> True
		Just sz -> n < sz
	
	nextchunksize n = case wantsize of
		Nothing -> defaultChunkSize
		Just sz -> 
			let togo = sz - n
			in if togo < toInteger defaultChunkSize
				then fromIntegral togo
				else defaultChunkSize

{- Same default chunk size Lazy ByteStrings use. -}
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
  where
	k = 1024
	chunkOverhead = 2 * sizeOf (1 :: Int) -- GHC specific

{- Runs an action, watching a file as it grows and updating the meter.
 -
 - The file may already exist, and the action could throw the original file
 - away and start over. To avoid reporting the original file size followed
 - by a smaller size in that case, wait until the file starts growing
 - before updating the meter for the first time.
 -}
watchFileSize :: (MonadIO m, MonadMask m) => FilePath -> MeterUpdate -> m a -> m a
watchFileSize f p a = bracket 
	(liftIO $ forkIO $ watcher =<< getsz)
	(liftIO . void . tryIO . killThread)
	(const a)
  where
	watcher oldsz = do
		threadDelay 500000 -- 0.5 seconds
		sz <- getsz
		when (sz > oldsz) $
			p sz
		watcher sz
	getsz = catchDefaultIO zeroBytesProcessed $
		toBytesProcessed <$> getFileSize f

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
				let s = encodeW8 (S.unpack b)
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

-- | Limit a meter to only update once per unit of time.
--
-- It's nice to display the final update to 100%, even if it comes soon
-- after a previous update. To make that happen, a total size has to be
-- provided.
rateLimitMeterUpdate :: NominalDiffTime -> Maybe Integer -> MeterUpdate -> IO MeterUpdate
rateLimitMeterUpdate delta totalsize meterupdate = do
	lastupdate <- newMVar (toEnum 0 :: POSIXTime)
	return $ mu lastupdate
  where
	mu lastupdate n@(BytesProcessed i) = case totalsize of
		Just t | i >= t -> meterupdate n
		_ -> do
			now <- getPOSIXTime
			prev <- takeMVar lastupdate
			if now - prev >= delta
				then do
					putMVar lastupdate now
					meterupdate n
				else putMVar lastupdate prev

data Meter = Meter (Maybe Integer) (MVar MeterState) (MVar String) RenderMeter DisplayMeter

type MeterState = (BytesProcessed, POSIXTime)

type DisplayMeter = MVar String -> String -> IO ()

type RenderMeter = Maybe Integer -> (BytesProcessed, POSIXTime) -> (BytesProcessed, POSIXTime) -> String

-- | Make a meter. Pass the total size, if it's known.
mkMeter :: Maybe Integer -> RenderMeter -> DisplayMeter -> IO Meter
mkMeter totalsize rendermeter displaymeter = Meter
	<$> pure totalsize
	<*> ((\t -> newMVar (zeroBytesProcessed, t)) =<< getPOSIXTime)
	<*> newMVar ""
	<*> pure rendermeter
	<*> pure displaymeter

-- | Updates the meter, displaying it if necessary.
updateMeter :: Meter -> BytesProcessed -> IO ()
updateMeter (Meter totalsize sv bv rendermeter displaymeter) new = do
	now <- getPOSIXTime
	(old, before) <- swapMVar sv (new, now)
	when (old /= new) $
		displaymeter bv $ 
			rendermeter totalsize (old, before) (new, now)

-- | Display meter to a Handle.
displayMeterHandle :: Handle -> DisplayMeter
displayMeterHandle h v s = do
	olds <- swapMVar v s
	-- Avoid writing when the rendered meter has not changed.
	when (olds /= s) $ do
		let padding = replicate (length olds - length s) ' '
		hPutStr h ('\r':s ++ padding)
		hFlush h

-- | Clear meter displayed by displayMeterHandle.
clearMeterHandle :: Meter -> Handle -> IO ()
clearMeterHandle (Meter _ _ v _ _) h = do
	olds <- readMVar v
	hPutStr h $ '\r' : replicate (length olds) ' ' ++ "\r"
	hFlush h

-- | Display meter in the form:
--   10%         300 KiB/s 16m40s
-- or when total size is not known:
--   1.3 MiB     300 KiB/s
bandwidthMeter :: RenderMeter
bandwidthMeter mtotalsize (BytesProcessed old, before) (BytesProcessed new, now) =
	unwords $ catMaybes
		[ Just percentoramount
		-- Pad enough for max width: "xxxx.xx KiB  xxxx KiB/s"
		, Just $ replicate (23 - length percentoramount - length rate) ' '
		, Just rate
		, estimatedcompletion
		]
  where
	percentoramount = case mtotalsize of
		Just totalsize -> showPercentage 0 $
			percentage totalsize (min new totalsize)
		Nothing -> roughSize' memoryUnits True 2 new
	rate = roughSize' memoryUnits True 0 bytespersecond ++ "/s"
	bytespersecond
		| duration == 0 = fromIntegral transferred
		| otherwise = floor $ fromIntegral transferred / duration
	transferred = max 0 (new - old)
	duration = max 0 (now - before)
	estimatedcompletion = case mtotalsize of
		Just totalsize
			| bytespersecond > 0 -> 
				Just $ fromDuration $ Duration $
					totalsize `div` bytespersecond
		_ -> Nothing

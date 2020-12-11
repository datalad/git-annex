{- Metered IO and actions
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}

module Utility.Metered (
	MeterUpdate,
	MeterState(..),
	nullMeterUpdate,
	combineMeterUpdate,
	TotalSize(..),
	BytesProcessed(..),
	toBytesProcessed,
	fromBytesProcessed,
	addBytesProcessed,
	zeroBytesProcessed,
	withMeteredFile,
	meteredWrite,
	meteredWrite',
	meteredWriteFile,
	offsetMeterUpdate,
	hGetContentsMetered,
	hGetMetered,
	defaultChunkSize,
	watchFileSize,
	OutputHandler(..),
	ProgressParser,
	commandMeter,
	commandMeter',
	commandMeterExitCode,
	commandMeterExitCode',
	demeterCommand,
	demeterCommandEnv,
	avoidProgress,
	rateLimitMeterUpdate,
	Meter,
	mkMeter,
	setMeterTotalSize,
	updateMeter,
	displayMeterHandle,
	clearMeterHandle,
	bandwidthMeter,
) where

import Common
import Utility.Percentage
import Utility.DataUnits
import Utility.HumanTime
import Utility.SimpleProtocol as Proto

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
	deriving (Eq, Ord, Show, Read)

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
				when (wantsize /= Just 0) $
					hClose h
				return L.empty
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
		toBytesProcessed <$> getFileSize f'
	f' = toRawFilePath f

data OutputHandler = OutputHandler
	{ quietMode :: Bool
	, stderrHandler :: String -> IO ()
	}

{- Parses the String looking for a command's progress output, and returns
 - Maybe the number of bytes done so far, optionally a total size, 
 - and any any remainder of the string that could be an incomplete
 - progress output. That remainder should be prepended to future output,
 - and fed back in. This interface allows the command's output to be read
 - in any desired size chunk, or even one character at a time.
 -}
type ProgressParser = String -> (Maybe BytesProcessed, Maybe TotalSize, String)

newtype TotalSize = TotalSize Integer
	deriving (Show, Eq)

{- Runs a command and runs a ProgressParser on its output, in order
 - to update a meter.
 -
 - If the Meter is provided, the ProgressParser can report the total size,
 - which allows creating a Meter before the size is known.
 -}
commandMeter :: ProgressParser -> OutputHandler -> Maybe Meter -> MeterUpdate -> FilePath -> [CommandParam] -> IO Bool
commandMeter progressparser oh meter meterupdate cmd params =
	commandMeter' progressparser oh meter meterupdate cmd params id

commandMeter' :: ProgressParser -> OutputHandler -> Maybe Meter -> MeterUpdate -> FilePath -> [CommandParam] -> (CreateProcess -> CreateProcess) -> IO Bool
commandMeter' progressparser oh meter meterupdate cmd params mkprocess = do
	ret <- commandMeterExitCode' progressparser oh meter meterupdate cmd params mkprocess
	return $ case ret of
		Just ExitSuccess -> True
		_ -> False

commandMeterExitCode :: ProgressParser -> OutputHandler -> Maybe Meter -> MeterUpdate -> FilePath -> [CommandParam] -> IO (Maybe ExitCode)
commandMeterExitCode progressparser oh meter meterupdate cmd params =
	commandMeterExitCode' progressparser oh meter meterupdate cmd params id

commandMeterExitCode' :: ProgressParser -> OutputHandler -> Maybe Meter -> MeterUpdate -> FilePath -> [CommandParam] -> (CreateProcess -> CreateProcess) -> IO (Maybe ExitCode)
commandMeterExitCode' progressparser oh mmeter meterupdate cmd params mkprocess = 
	outputFilter cmd params mkprocess Nothing
		(const $ feedprogress mmeter zeroBytesProcessed [])
		handlestderr
  where
	feedprogress sendtotalsize prev buf h = do
		b <- S.hGetSome h 80
		if S.null b
			then return ()
			else do
				unless (quietMode oh) $ do
					S.hPut stdout b
					hFlush stdout
				let s = decodeBS b
				let (mbytes, mtotalsize, buf') = progressparser (buf++s)
				sendtotalsize' <- case (sendtotalsize, mtotalsize) of
					(Just meter, Just t) -> do
						setMeterTotalSize meter t
						return Nothing
					_ -> return sendtotalsize
				case mbytes of
					Nothing -> feedprogress sendtotalsize' prev buf' h
					(Just bytes) -> do
						when (bytes /= prev) $
							meterupdate bytes
						feedprogress sendtotalsize' bytes buf' h

	handlestderr ph h = hGetLineUntilExitOrEOF ph h >>= \case
		Just l -> do
			stderrHandler oh l
			handlestderr ph h
		Nothing -> return ()

{- Runs a command, that may display one or more progress meters on
 - either stdout or stderr, and prevents the meters from being displayed.
 -
 - The other command output is handled as configured by the OutputHandler.
 -}
demeterCommand :: OutputHandler -> FilePath -> [CommandParam] -> IO Bool
demeterCommand oh cmd params = demeterCommandEnv oh cmd params Nothing

demeterCommandEnv :: OutputHandler -> FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
demeterCommandEnv oh cmd params environ = do
	ret <- outputFilter cmd params id environ
		(\ph outh -> avoidProgress True ph outh stdouthandler)
		(\ph errh -> avoidProgress True ph errh $ stderrHandler oh)
	return $ case ret of
		Just ExitSuccess -> True
		_ -> False
  where
	stdouthandler l = 
		unless (quietMode oh) $
			putStrLn l

{- To suppress progress output, while displaying other messages,
 - filter out lines that contain \r (typically used to reset to the
 - beginning of the line when updating a progress display).
 -}
avoidProgress :: Bool -> ProcessHandle -> Handle -> (String -> IO ()) -> IO ()
avoidProgress doavoid ph h emitter = hGetLineUntilExitOrEOF ph h >>= \case
	Just s -> do
		unless (doavoid && '\r' `elem` s) $
			emitter s
		avoidProgress doavoid ph h emitter
	Nothing -> return ()

outputFilter
	:: FilePath
	-> [CommandParam]
	-> (CreateProcess -> CreateProcess)
	-> Maybe [(String, String)]
	-> (ProcessHandle -> Handle -> IO ())
	-> (ProcessHandle -> Handle -> IO ())
	-> IO (Maybe ExitCode)
outputFilter cmd params mkprocess environ outfilter errfilter = 
	catchMaybeIO $ withCreateProcess p go
  where
	go _ (Just outh) (Just errh) ph = do
		outt <- async $ tryIO (outfilter ph outh) >> hClose outh
		errt <- async $ tryIO (errfilter ph errh) >> hClose errh
		ret <- waitForProcess ph
		wait outt
		wait errt
		return ret
	go _ _ _ _ = error "internal"
	
	p = mkprocess (proc cmd (toCommand params))
		{ env = environ
		, std_out = CreatePipe
		, std_err = CreatePipe
		}

-- | Limit a meter to only update once per unit of time.
--
-- It's nice to display the final update to 100%, even if it comes soon
-- after a previous update. To make that happen, the Meter has to know
-- its total size.
rateLimitMeterUpdate :: NominalDiffTime -> Meter -> MeterUpdate -> IO MeterUpdate
rateLimitMeterUpdate delta (Meter totalsizev _ _ _) meterupdate = do
	lastupdate <- newMVar (toEnum 0 :: POSIXTime)
	return $ mu lastupdate
  where
	mu lastupdate n@(BytesProcessed i) = readMVar totalsizev >>= \case
		Just (TotalSize t) | i >= t -> meterupdate n
		_ -> do
			now <- getPOSIXTime
			prev <- takeMVar lastupdate
			if now - prev >= delta
				then do
					putMVar lastupdate now
					meterupdate n
				else putMVar lastupdate prev

data Meter = Meter (MVar (Maybe TotalSize)) (MVar MeterState) (MVar String) DisplayMeter

data MeterState = MeterState
	{ meterBytesProcessed :: BytesProcessed
	, meterTimeStamp :: POSIXTime
	} deriving (Show)

type DisplayMeter = MVar String -> Maybe TotalSize -> MeterState -> MeterState -> IO ()

type RenderMeter = Maybe TotalSize -> MeterState -> MeterState -> String

-- | Make a meter. Pass the total size, if it's known.
mkMeter :: Maybe TotalSize -> DisplayMeter -> IO Meter
mkMeter totalsize displaymeter = do
	ts <- getPOSIXTime
	Meter
		<$> newMVar totalsize
		<*> newMVar (MeterState zeroBytesProcessed ts)
		<*> newMVar ""
		<*> pure displaymeter

setMeterTotalSize :: Meter -> TotalSize -> IO ()
setMeterTotalSize (Meter totalsizev _ _ _) = void . swapMVar totalsizev . Just

-- | Updates the meter, displaying it if necessary.
updateMeter :: Meter -> MeterUpdate
updateMeter (Meter totalsizev sv bv displaymeter) new = do
	now <- getPOSIXTime
	let curms = MeterState new now
	oldms <- swapMVar sv curms
	when (meterBytesProcessed oldms /= new) $ do
		totalsize <- readMVar totalsizev
		displaymeter bv totalsize oldms curms

-- | Display meter to a Handle.
displayMeterHandle :: Handle -> RenderMeter -> DisplayMeter
displayMeterHandle h rendermeter v msize old new = do
	let s = rendermeter msize old new
	olds <- swapMVar v s
	-- Avoid writing when the rendered meter has not changed.
	when (olds /= s) $ do
		let padding = replicate (length olds - length s) ' '
		hPutStr h ('\r':s ++ padding)
		hFlush h

-- | Clear meter displayed by displayMeterHandle.
clearMeterHandle :: Meter -> Handle -> IO ()
clearMeterHandle (Meter _ _ v _) h = do
	olds <- readMVar v
	hPutStr h $ '\r' : replicate (length olds) ' ' ++ "\r"
	hFlush h

-- | Display meter in the form:
--   10%  1.3MiB  300 KiB/s 16m40s
-- or when total size is not known:
--   1.3 MiB      300 KiB/s
bandwidthMeter :: RenderMeter
bandwidthMeter mtotalsize (MeterState (BytesProcessed old) before) (MeterState (BytesProcessed new) now) =
	unwords $ catMaybes
		[ Just percentamount
		-- Pad enough for max width: "100%  xxxx.xx KiB  xxxx KiB/s"
		, Just $ replicate (29 - length percentamount - length rate) ' '
		, Just rate
		, estimatedcompletion
		]
  where
	amount = roughSize' memoryUnits True 2 new
	percentamount = case mtotalsize of
		Just (TotalSize totalsize) ->
			let p = showPercentage 0 $
				percentage totalsize (min new totalsize)
			in p ++ replicate (6 - length p) ' ' ++ amount
		Nothing -> amount
	rate = roughSize' memoryUnits True 0 bytespersecond ++ "/s"
	bytespersecond
		| duration == 0 = fromIntegral transferred
		| otherwise = floor $ fromIntegral transferred / duration
	transferred = max 0 (new - old)
	duration = max 0 (now - before)
	estimatedcompletion = case mtotalsize of
		Just (TotalSize totalsize)
			| bytespersecond > 0 -> 
				Just $ fromDuration $ Duration $
					(totalsize - new) `div` bytespersecond
		_ -> Nothing

instance Proto.Serializable BytesProcessed where
	serialize (BytesProcessed n) = show n
	deserialize = BytesProcessed <$$> readish

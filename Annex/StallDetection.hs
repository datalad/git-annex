{- Stall detection for transfers.
 -
 - Copyright 2020-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.StallDetection (detectStalls, StallDetection) where

import Annex.Common
import Types.StallDetection
import Utility.Metered
import Utility.HumanTime
import Utility.DataUnits
import Utility.ThreadScheduler

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock

{- This may be safely canceled (with eg uninterruptibleCancel),
 - as long as the passed action can be safely canceled. -}
detectStalls :: (Monad m, MonadIO m) => Maybe StallDetection -> TVar (Maybe BytesProcessed) -> m () -> m ()
detectStalls Nothing _ _ = noop
detectStalls (Just StallDetectionDisabled) _ _ = noop
detectStalls (Just (StallDetection (BwRate minsz duration))) metervar onstall = do
	-- If the progress is being updated, but less frequently than
	-- the specified duration, a stall would be incorrectly detected.
	--
	-- For example, consider the case of a remote that does
	-- not support progress updates, but is chunked with a large chunk
	-- size. In that case, progress is only updated after each chunk.
	--
	-- So, wait for the first update, and see how long it takes.
	-- It's longer than the duration, upscale the duration and minsz
	-- accordingly.
	starttime <- liftIO getCurrentTime
	v <- waitforfirstupdate =<< readMeterVar metervar
	endtime <- liftIO getCurrentTime
	let timepassed = floor (endtime `diffUTCTime` starttime)
	let (scaledminsz, scaledduration) = upscale timepassed
	detectStalls' scaledminsz scaledduration metervar onstall v
  where
	upscale timepassed
		| timepassed > dsecs = 
			let scale = scaleamount timepassed
			in (minsz * scale, Duration (dsecs * scale))
		| otherwise = (minsz, duration)
	scaleamount timepassed = max 1 $ floor $
		(fromIntegral timepassed / fromIntegral (max dsecs 1))
		* scalefudgefactor
	scalefudgefactor = 1.5 :: Double
	dsecs = durationSeconds duration
	minwaitsecs = Seconds $
		min 60 (fromIntegral (durationSeconds duration))
	waitforfirstupdate startval = do
		liftIO $ threadDelaySeconds minwaitsecs
		v <- readMeterVar metervar
		if v > startval
			then return v
			else waitforfirstupdate startval
	
detectStalls (Just ProbeStallDetection) metervar onstall = do
	-- Only do stall detection once the progress is confirmed to be
	-- consistently updating. After the first update, it needs to
	-- advance twice within 30 seconds. With that established,
	-- if no data at all is sent for a 60 second period, it's
	-- assumed to be a stall.
	v <- readMeterVar metervar >>= waitforfirstupdate
	ontimelyadvance v $ \v' -> ontimelyadvance v' $
		detectStalls' 1 duration metervar onstall
  where
	duration = Duration 60

	delay = Seconds (fromIntegral (durationSeconds duration) `div` 2)
	
	waitforfirstupdate startval = do
		liftIO $ threadDelaySeconds delay
		v <- readMeterVar metervar
		if v > startval
			then return v
			else waitforfirstupdate startval

	ontimelyadvance v cont = do
		liftIO $ threadDelaySeconds delay
		v' <- readMeterVar metervar
		when (v' > v) $
			cont v'

detectStalls'
	:: (Monad m, MonadIO m)
	=> ByteSize
	-> Duration
	-> TVar (Maybe BytesProcessed)
	-> m ()
	-> Maybe ByteSize
	-> m ()
detectStalls' minsz duration metervar onstall st = do
	liftIO $ threadDelaySeconds delay
	-- Get whatever progress value was reported most recently, if any.
	v <- readMeterVar metervar
	let cont = detectStalls' minsz duration metervar onstall v
	case (st, v) of
		(Nothing, _) -> cont
		(_, Nothing) -> cont
		(Just prev, Just sofar)
			-- Just in case a progress meter somehow runs
			-- backwards, or a second progress meter was
			-- started and is at a smaller value than
			-- the previous one.
			| prev > sofar -> cont
			| sofar - prev < minsz -> onstall
			| otherwise -> cont
  where
	delay = Seconds (fromIntegral (durationSeconds duration))

readMeterVar
	:: MonadIO m
	=> TVar (Maybe BytesProcessed)
	-> m (Maybe ByteSize)
readMeterVar metervar = liftIO $ atomically $ 
	fmap fromBytesProcessed <$> readTVar metervar

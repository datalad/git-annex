{- Stall detection for transfers.
 -
 - Copyright 2020-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.StallDetection (
	getStallDetection,
	detectStalls,
	StallDetection,
) where

import Annex.Common
import Types.StallDetection
import Types.Direction
import Types.Remote (gitconfig)
import Utility.Metered
import Utility.HumanTime
import Utility.DataUnits
import Utility.ThreadScheduler

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock

getStallDetection :: Direction -> Remote -> Maybe StallDetection
getStallDetection Download r = 
	remoteAnnexStallDetectionDownload (gitconfig r)
		<|> remoteAnnexStallDetection (gitconfig r)
getStallDetection Upload r =
	remoteAnnexStallDetectionUpload (gitconfig r)
		<|> remoteAnnexStallDetection (gitconfig r)

{- This may be safely canceled (with eg uninterruptibleCancel),
 - as long as the passed action can be safely canceled. -}
detectStalls :: (Monad m, MonadIO m) => Maybe StallDetection -> TVar (Maybe BytesProcessed) -> m () -> m ()
detectStalls Nothing _ _ = noop
detectStalls (Just StallDetectionDisabled) _ _ = noop
detectStalls (Just (StallDetection bwrate@(BwRate _minsz duration))) metervar onstall = do
	-- If the progress is being updated, but less frequently than
	-- the specified duration, a stall would be incorrectly detected.
	--
	-- For example, consider the case of a remote that does
	-- not support progress updates, but is chunked with a large chunk
	-- size. In that case, progress is only updated after each chunk.
	--
	-- So, wait for the first update, and see how long it takes.
	-- When it's longer than the duration (or close to it), 
	-- upscale the duration and minsz accordingly.
	starttime <- liftIO getCurrentTime
	v <- waitforfirstupdate =<< readMeterVar metervar
	endtime <- liftIO getCurrentTime
	let timepassed = floor (endtime `diffUTCTime` starttime)
	let BwRate scaledminsz scaledduration = upscale bwrate timepassed
	detectStalls' scaledminsz scaledduration metervar onstall v
  where
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

-- Scale up the minsz and duration to match the observed time that passed
-- between progress updates. This allows for some variation in the transfer
-- rate causing later progress updates to happen less frequently.
upscale :: BwRate -> Integer -> BwRate
upscale input@(BwRate minsz duration) timepassedsecs
	| timepassedsecs > dsecs `div` allowedvariation = BwRate 
		(ceiling (fromIntegral minsz * scale))
		(Duration (ceiling (fromIntegral dsecs * scale)))
	| otherwise = input
  where
	scale = max (1 :: Double) $
		(fromIntegral timepassedsecs / fromIntegral (max dsecs 1))
		* fromIntegral allowedvariation
	
	dsecs = durationSeconds duration

	-- Setting this too low will make normal bandwidth variations be
	-- considered to be stalls, while setting it too high will make
	-- stalls not be detected for much longer than the expected
	-- duration.
	--
	-- For example, a BwRate of 20MB/1m, when the first progress
	-- update takes 10m to arrive, is scaled to 600MB/30m. That 30m
	-- is a reasonable since only 3 chunks get sent in that amount of
	-- time at that rate. If allowedvariation = 10, that would
	-- be 2000MB/100m, which seems much too long to wait to detect a
	-- stall.
	allowedvariation = 3

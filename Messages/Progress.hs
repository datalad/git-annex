{- git-annex progress output
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Messages.Progress where

import Common
import qualified Annex
import Messages
import Utility.Metered
import Types
import Types.Messages
import Types.Key
import Types.KeySource
import Types.StallDetection (BwRate(..))
import Utility.InodeCache
import qualified Messages.JSON as JSON
import Messages.Concurrent
import Messages.Internal
import Utility.SafeOutput

import qualified System.Console.Regions as Regions
import qualified System.Console.Concurrent as Console
import Control.Monad.IO.Class (MonadIO)
import Data.IORef

{- Class of things from which a size can be gotten to display a progress
 - meter. -}
class MeterSize t where
	getMeterSize :: t -> Annex (Maybe TotalSize)

instance MeterSize t => MeterSize (Maybe t) where
	getMeterSize Nothing = pure Nothing
	getMeterSize (Just t) = getMeterSize t

instance MeterSize FileSize where
	getMeterSize = pure . Just . TotalSize

instance MeterSize Key where
	getMeterSize = pure . fmap TotalSize . fromKey keySize

instance MeterSize InodeCache where
	getMeterSize = pure . Just . TotalSize . inodeCacheFileSize

instance MeterSize KeySource where
	getMeterSize = maybe (pure Nothing) getMeterSize . inodeCache

{- When the key's size is not known, the file is statted to get the size.
 - This allows uploads of keys without size to still have progress
 - displayed.
 -}
data KeySizer = KeySizer Key (Annex (Maybe OsPath))

instance MeterSize KeySizer where
	getMeterSize (KeySizer k getsrcfile) = case fromKey keySize k of
		Just sz -> return (Just (TotalSize sz))
		Nothing -> do
			srcfile <- getsrcfile
			case srcfile of
				Nothing -> return Nothing
				Just f -> catchMaybeIO $ liftIO $
					TotalSize <$> getFileSize f

{- Shows a progress meter while performing an action.
 - The action is passed the meter and a callback to use to update the meter.
 -}
metered
	:: MeterSize sizer
	=> Maybe MeterUpdate
	-> sizer
	-> Maybe BwRate
	-> (Meter -> MeterUpdate -> Annex a)
	-> Annex a
metered othermeterupdate sizer bwlimit a = withMessageState $ \st -> do
	sz <- getMeterSize sizer
	metered' st setclear othermeterupdate sz bwlimit showOutput a
  where
	setclear c = Annex.changeState $ \st -> st
		{ Annex.output = (Annex.output st) { clearProgressMeter = c } }

metered'
	:: (Monad m, MonadIO m, MonadMask m)
	=> MessageState
	-> (IO () -> m ())
	-- ^ This should set clearProgressMeter when progress meters
	-- are being displayed; not needed when outputType is not
	-- NormalOutput.
	-> Maybe MeterUpdate
	-> Maybe TotalSize
	-> Maybe BwRate
	-> m ()
	-- ^ this should run showOutput
	-> (Meter -> MeterUpdate -> m a)
	-> m a
metered' st setclear othermeterupdate msize bwlimit showoutput a = go st
  where
	go (MessageState { outputType = QuietOutput }) = nometer
	go (MessageState { outputType = NormalOutput, concurrentOutputEnabled = False }) = do
		showoutput
		meter <- liftIO $ mkMeter msize $ 
			displayMeterHandle stdout bandwidthMeter
		let clear = clearMeterHandle meter stdout
		setclear clear
		m <- liftIO $ rateLimitMeterUpdate consoleratelimit meter $
			updateMeter meter
		r <- a meter =<< mkmeterupdate m
		setclear noop
		liftIO clear
		return r
	go (MessageState { outputType = NormalOutput, concurrentOutputEnabled = True }) =
		withProgressRegion st $ \r -> do
			meter <- liftIO $ mkMeter msize $ \_ msize' old new ->
				let s = bandwidthMeter msize' old new
				in Regions.setConsoleRegion r ('\n' : s)
			m <- liftIO $ rateLimitMeterUpdate consoleratelimit meter $
				updateMeter meter
			a meter =<< mkmeterupdate m
	go (MessageState { outputType = JSONOutput jsonoptions })
		| jsonProgress jsonoptions = do
			let buf = jsonBuffer st
			meter <- liftIO $ mkMeter msize $ \_ msize' _old new ->
				JSON.progress buf msize' (meterBytesProcessed new)
			m <- liftIO $ rateLimitMeterUpdate jsonratelimit meter $
				updateMeter meter
			a meter =<< mkmeterupdate m
		| otherwise = nometer
	go (MessageState { outputType = SerializedOutput h _ }) = do
		liftIO $ outputSerialized h BeginProgressMeter
		case msize of
			Just sz -> liftIO $ outputSerialized h $ UpdateProgressMeterTotalSize sz
			Nothing -> noop
		szv <- liftIO $ newIORef msize
		meter <- liftIO $ mkMeter msize $ \_ msize' _old new -> do
			case msize' of
				Just sz | msize' /= msize -> do
					psz <- readIORef szv
					when (msize' /= psz) $ do
						writeIORef szv msize'
						outputSerialized h $ UpdateProgressMeterTotalSize sz
				_ -> noop
			outputSerialized h $ UpdateProgressMeter $
				meterBytesProcessed new
		m <- liftIO $ rateLimitMeterUpdate minratelimit meter $
			updateMeter meter
		(a meter =<< mkmeterupdate m)
			`finally` (liftIO $ outputSerialized h EndProgressMeter)
	nometer = do
		dummymeter <- liftIO $ mkMeter Nothing $
			\_ _ _ _ -> return ()
		a dummymeter =<< mkmeterupdate (const noop)

	mkmeterupdate m = 
		let mu = case othermeterupdate of
			Nothing -> m
			Just om -> combineMeterUpdate m om
		in case bwlimit of
			Nothing -> return mu
			Just (BwRate sz duration) -> liftIO $
				bwLimitMeterUpdate sz duration mu

	consoleratelimit = 0.2

	jsonratelimit = 0.1

	minratelimit = min consoleratelimit jsonratelimit
		
{- Poll file size to display meter. -}
meteredFile :: OsPath -> Maybe MeterUpdate -> Key -> (MeterUpdate -> Annex a) -> Annex a
meteredFile file combinemeterupdate key a = 
	metered combinemeterupdate key Nothing $ \_ p ->
		watchFileSize file p a

{- Progress dots. -}
showProgressDots :: Annex ()
showProgressDots = outputMessage JSON.none id "."

{- Runs a command, that may output progress to either stdout or
 - stderr, as well as other messages.
 -
 - In quiet mode, the output is suppressed, except for error messages.
 -}
progressCommand :: FilePath -> [CommandParam] -> Annex Bool
progressCommand cmd params = progressCommandEnv cmd params Nothing

progressCommandEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> Annex Bool
progressCommandEnv cmd params environ = ifM commandProgressDisabled
	( do
		oh <- mkOutputHandler
		liftIO $ demeterCommandEnv oh cmd params environ
	, liftIO $ boolSystemEnv cmd params environ
	)

mkOutputHandler :: Annex OutputHandler
mkOutputHandler = OutputHandler
	<$> commandProgressDisabled
	<*> mkStderrEmitter

mkOutputHandlerQuiet :: Annex OutputHandler
mkOutputHandlerQuiet = OutputHandler
	<$> pure True
	<*> mkStderrEmitter

mkStderrRelayer :: Annex (ProcessHandle -> Handle -> IO ())
mkStderrRelayer = do
	quiet <- commandProgressDisabled
	emitter <- mkStderrEmitter
	return $ \ph h -> avoidProgress quiet ph h emitter

{- Generates an IO action that can be used to emit stderr.
 -
 - When a progress meter is displayed, this takes care to avoid
 - messing it up with interleaved stderr from a command.
 -}
mkStderrEmitter :: Annex (String -> IO ())
mkStderrEmitter = withMessageState go
  where
	go s
		| concurrentOutputEnabled s = return (Console.errorConcurrent . safeOutput)
		| otherwise = return (hPutStrLn stderr . safeOutput)

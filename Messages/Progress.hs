{- git-annex progress output
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Messages.Progress where

import Common
import Messages
import Utility.Metered
import Types
import Types.Messages
import Types.Key
import Types.KeySource
import Utility.InodeCache
import qualified Messages.JSON as JSON
import Messages.Concurrent

import qualified System.Console.Regions as Regions
import qualified System.Console.Concurrent as Console

{- Class of things from which a size can be gotten to display a progress
 - meter. -}
class MeterSize t where
	getMeterSize :: t -> Annex (Maybe FileSize)

instance MeterSize t => MeterSize (Maybe t) where
	getMeterSize Nothing = pure Nothing
	getMeterSize (Just t) = getMeterSize t

instance MeterSize FileSize where
	getMeterSize = pure . Just

instance MeterSize Key where
	getMeterSize = pure . fromKey keySize

instance MeterSize InodeCache where
	getMeterSize = pure . Just . inodeCacheFileSize

instance MeterSize KeySource where
	getMeterSize = maybe (pure Nothing) getMeterSize . inodeCache

{- When the key's size is not known, the file is statted to get the size.
 - This allows uploads of keys without size to still have progress
 - displayed.
 -}
data KeySizer = KeySizer Key (Annex (Maybe FilePath))

instance MeterSize KeySizer where
	getMeterSize (KeySizer k getsrcfile) = case fromKey keySize k of
		Just sz -> return (Just sz)
		Nothing -> do
			srcfile <- getsrcfile
			case srcfile of
				Nothing -> return Nothing
				Just f -> catchMaybeIO $ liftIO $ getFileSize f

{- Shows a progress meter while performing an action.
 - The action is passed the meter and a callback to use to update the meter.
 --}
metered :: MeterSize sizer => Maybe MeterUpdate -> sizer -> (Meter -> MeterUpdate -> Annex a) -> Annex a
metered othermeter sizer a = withMessageState $ \st ->
	flip go st =<< getMeterSize sizer
  where
	go _ (MessageState { outputType = QuietOutput }) = nometer
	go msize (MessageState { outputType = NormalOutput, concurrentOutputEnabled = False }) = do
		showOutput
		meter <- liftIO $ mkMeter msize $ 
			displayMeterHandle stdout bandwidthMeter
		m <- liftIO $ rateLimitMeterUpdate 0.2 meter $
			updateMeter meter
		r <- a meter (combinemeter m)
		liftIO $ clearMeterHandle meter stdout
		return r
	go msize (MessageState { outputType = NormalOutput, concurrentOutputEnabled = True }) =
		withProgressRegion $ \r -> do
			meter <- liftIO $ mkMeter msize $ \_ msize' old new ->
				let s = bandwidthMeter msize' old new
				in Regions.setConsoleRegion r ('\n' : s)
			m <- liftIO $ rateLimitMeterUpdate 0.2 meter $
				updateMeter meter
			a meter (combinemeter m)
	go msize (MessageState { outputType = JSONOutput jsonoptions })
		| jsonProgress jsonoptions = do
			buf <- withMessageState $ return . jsonBuffer
			meter <- liftIO $ mkMeter msize $ \_ msize' _old (new, _now) ->
				JSON.progress buf msize' new
			m <- liftIO $ rateLimitMeterUpdate 0.1 meter $
				updateMeter meter
			a meter (combinemeter m)
		| otherwise = nometer

	nometer = do
		dummymeter <- liftIO $ mkMeter Nothing $
			\_ _ _ _ -> return ()
		a dummymeter (combinemeter (const noop))

	combinemeter m = case othermeter of
		Nothing -> m
		Just om -> combineMeterUpdate m om

{- Poll file size to display meter. -}
meteredFile :: FilePath -> Maybe MeterUpdate -> Key -> Annex a -> Annex a
meteredFile file combinemeterupdate key a = 
	metered combinemeterupdate key $ \_ p ->
		watchFileSize file p a

{- Progress dots. -}
showProgressDots :: Annex ()
showProgressDots = outputMessage JSON.none "."

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

mkStderrRelayer :: Annex (Handle -> IO ())
mkStderrRelayer = do
	quiet <- commandProgressDisabled
	emitter <- mkStderrEmitter
	return $ \h -> avoidProgress quiet h emitter

{- Generates an IO action that can be used to emit stderr.
 -
 - When a progress meter is displayed, this takes care to avoid
 - messing it up with interleaved stderr from a command.
 -}
mkStderrEmitter :: Annex (String -> IO ())
mkStderrEmitter = withMessageState go
  where
	go s
		| concurrentOutputEnabled s = return Console.errorConcurrent
		| otherwise = return (hPutStrLn stderr)

{- git-annex progress output
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Messages.Progress where

import Common
import Messages
import Utility.Metered
import Types
import Types.Messages
import Types.Key
import qualified Messages.JSON as JSON

#ifdef WITH_CONCURRENTOUTPUT
import Messages.Concurrent
import qualified System.Console.Regions as Regions
import qualified System.Console.Concurrent as Console
#endif

{- Shows a progress meter while performing a transfer of a key.
 - The action is passed the meter and a callback to use to update the meter.
 -
 - When the key's size is not known, the srcfile is statted to get the size.
 - This allows uploads of keys without size to still have progress
 - displayed.
 --}
metered :: Maybe MeterUpdate -> Key -> Annex (Maybe FilePath) -> (Meter -> MeterUpdate -> Annex a) -> Annex a
metered othermeter key getsrcfile a = withMessageState $ \st ->
	flip go st =<< getsz
  where
	go _ (MessageState { outputType = QuietOutput }) = nometer
	go msize (MessageState { outputType = NormalOutput, concurrentOutputEnabled = False }) = do
		showOutput
		meter <- liftIO $ mkMeter msize $ 
			displayMeterHandle stdout bandwidthMeter
		m <- liftIO $ rateLimitMeterUpdate 0.1 meter $
			updateMeter meter
		r <- a meter (combinemeter m)
		liftIO $ clearMeterHandle meter stdout
		return r
	go msize (MessageState { outputType = NormalOutput, concurrentOutputEnabled = True }) =
#if WITH_CONCURRENTOUTPUT
		withProgressRegion $ \r -> do
			meter <- liftIO $ mkMeter msize $ \_ msize' old new ->
				let s = bandwidthMeter msize' old new
				in Regions.setConsoleRegion r ('\n' : s)
			m <- liftIO $ rateLimitMeterUpdate 0.1 meter $
				updateMeter meter
			a meter (combinemeter m)
#else
		nometer
#endif
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
	
	getsz = case keySize key of
		Just sz -> return (Just sz)
		Nothing -> do
			srcfile <- getsrcfile
			case srcfile of
				Nothing -> return Nothing
				Just f -> catchMaybeIO $ liftIO $ getFileSize f

{- Poll file size to display meter, but only when concurrent output or
 - json progress needs the information. -}
meteredFile :: FilePath -> Maybe MeterUpdate -> Key -> Annex a -> Annex a
meteredFile file combinemeterupdate key a = 
	withMessageState $ \s -> if needOutputMeter s
		then metered combinemeterupdate key (return Nothing) $ \_ p ->
			watchFileSize file p a
		else a
  where
	needOutputMeter s = case outputType s of
		JSONOutput jsonoptions -> jsonProgress jsonoptions
		NormalOutput | concurrentOutputEnabled s -> True
		_ -> False

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
#ifdef WITH_CONCURRENTOUTPUT
	go s | concurrentOutputEnabled s = return Console.errorConcurrent
#endif
	go _ = return (hPutStrLn stderr)

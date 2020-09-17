{- Interface to libmagic
 -
 - Copyright 2019-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Magic (
	Magic,
	MimeType,
	MimeEncoding,
	initMagicMime,
	getMagicMimeType,
	getMagicMimeEncoding,
) where

import Types.Mime
import Control.Monad.IO.Class
#ifdef WITH_MAGICMIME
import Magic
import Utility.Env
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Common
#else
type Magic = ()
#endif

initMagicMime :: IO (Maybe Magic)
#ifdef WITH_MAGICMIME
initMagicMime = catchMaybeIO $ do
	m <- magicOpen [MagicMime]
	liftIO $ getEnv "GIT_ANNEX_DIR" >>= \case
		Nothing -> magicLoadDefault m
		Just d -> magicLoad m
			(d </> "magic" </> "magic.mgc")
	return m
#else
initMagicMime = return Nothing
#endif

getMagicMime :: Magic -> FilePath -> IO (Maybe (MimeType, MimeEncoding))
#ifdef WITH_MAGICMIME
getMagicMime m f = Just . parse <$> magicConcurrentSafe (magicFile m f)
  where
	parse s = 
		let (mimetype, rest) = separate (== ';') s
		in case rest of
			(' ':'c':'h':'a':'r':'s':'e':'t':'=':mimeencoding) -> 
				(mimetype, mimeencoding)
			_ -> (mimetype, "")
#else
getMagicMime _ _ = return Nothing
#endif

getMagicMimeType :: MonadIO m => Magic -> FilePath -> m (Maybe MimeType)
getMagicMimeType m f = liftIO $ fmap fst <$> getMagicMime m f

getMagicMimeEncoding :: MonadIO m => Magic -> FilePath -> m(Maybe MimeEncoding)
getMagicMimeEncoding m f = liftIO $ fmap snd <$> getMagicMime m f

#ifdef WITH_MAGICMIME
{-# NOINLINE mutex #-}
mutex :: MVar ()
mutex = unsafePerformIO $ newMVar ()

-- Work around a bug, the library is not concurrency safe and will
-- sometimes access the wrong memory if multiple ones are called at the
-- same time.
magicConcurrentSafe :: IO a -> IO a
magicConcurrentSafe = bracket_ (takeMVar mutex) (putMVar mutex ())
#endif

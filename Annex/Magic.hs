{- Interface to libmagic
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Magic (
	Magic,
	MimeType,
	initMagicMimeType,
	getMagicMimeType,
) where

#ifdef WITH_MAGICMIME
import Magic
import Utility.Env
import Common
#else
type Magic = ()
#endif

initMagicMimeType :: IO (Maybe Magic)
#ifdef WITH_MAGICMIME
initMagicMimeType = catchMaybeIO $ do
	m <- magicOpen [MagicMimeType]
	liftIO $ getEnv "GIT_ANNEX_DIR" >>= \case
		Nothing -> magicLoadDefault m
		Just d -> magicLoad m
			(d </> "magic" </> "magic.mgc")
	return m
#else
initMagicMimeType = return Nothing
#endif

type MimeType = String

getMagicMimeType :: Magic -> FilePath -> IO (Maybe MimeType)
#ifdef WITH_MAGICMIME
getMagicMimeType m f = Just <$> magicFile m f
#else
getMagicMimeType _ _ = return Nothing
#endif

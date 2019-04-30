{- Interface to libmagic
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
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

#ifdef WITH_MAGICMIME
import Magic
import Utility.Env
import Common
#else
type Magic = ()
#endif
import Types.Mime

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
getMagicMime m f = Just . parse <$> magicFile m f
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

getMagicMimeType :: Magic -> FilePath -> IO (Maybe MimeType)
getMagicMimeType m f = fmap fst <$> getMagicMime m f

getMagicMimeEncoding :: Magic -> FilePath -> IO (Maybe MimeEncoding)
getMagicMimeEncoding m f = fmap fst <$> getMagicMime m f

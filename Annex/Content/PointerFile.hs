{- git-annex pointer files
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content.PointerFile where

#if ! defined(mingw32_HOST_OS)
import System.Posix.Files
#else
import System.PosixCompat.Files
#endif

import Annex.Common
import Annex.Perms
import Annex.Link
import Annex.ReplaceFile
import Annex.InodeSentinal
import Annex.Content.LowLevel
import Utility.InodeCache
#if ! defined(mingw32_HOST_OS)
import Utility.Touch
#endif

{- Populates a pointer file with the content of a key. 
 -
 - If the file already has some other content, it is not modified.
 -
 - Returns an InodeCache if it populated the pointer file.
 -}
populatePointerFile :: Restage -> Key -> RawFilePath -> RawFilePath -> Annex (Maybe InodeCache)
populatePointerFile restage k obj f = go =<< liftIO (isPointerFile f)
  where
	go (Just k') | k == k' = do
		let f' = fromRawFilePath f
		destmode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus f'
		liftIO $ nukeFile f'
		(ic, populated) <- replaceFile f' $ \tmp -> do
			let tmp' = toRawFilePath tmp
			ok <- linkOrCopy k (fromRawFilePath obj) tmp destmode >>= \case
				Just _ -> thawContent tmp >> return True
				Nothing -> liftIO (writePointerFile tmp' k destmode) >> return False
			ic <- withTSDelta (liftIO . genInodeCache tmp')
			return (ic, ok)
		maybe noop (restagePointerFile restage f) ic
		if populated
			then return ic
			else return Nothing
	go _ = return Nothing
	
{- Removes the content from a pointer file, replacing it with a pointer.
 -
 - Does not check if the pointer file is modified. -}
depopulatePointerFile :: Key -> RawFilePath -> Annex ()
depopulatePointerFile key file = do
	let file' = fromRawFilePath file
	st <- liftIO $ catchMaybeIO $ getFileStatus file'
	let mode = fmap fileMode st
	secureErase file'
	liftIO $ nukeFile file'
	ic <- replaceFile file' $ \tmp -> do
		liftIO $ writePointerFile (toRawFilePath tmp) key mode
#if ! defined(mingw32_HOST_OS)
		-- Don't advance mtime; this avoids unncessary re-smudging
		-- by git in some cases.
		liftIO $ maybe noop
			(\t -> touch tmp t False)
			(fmap modificationTimeHiRes st)
#endif
		withTSDelta (liftIO . genInodeCache (toRawFilePath tmp))
	maybe noop (restagePointerFile (Restage True) file) ic

{- git-annex pointer files
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content.PointerFile where

import System.PosixCompat.Files

import Annex.Common
import Annex.Perms
import Annex.Link
import Annex.ReplaceFile
import Annex.InodeSentinal
import Utility.InodeCache
import Annex.Content.LowLevel

{- Populates a pointer file with the content of a key. -}
populatePointerFile :: Restage -> Key -> FilePath -> FilePath -> Annex ()
populatePointerFile restage k obj f = go =<< liftIO (isPointerFile f)
  where
	go (Just k') | k == k' = do
		destmode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus f
		liftIO $ nukeFile f
		ic <- replaceFile f $ \tmp -> do
			ifM (linkOrCopy k obj tmp destmode)
				( thawContent tmp
				, liftIO $ writePointerFile tmp k destmode
				)
			withTSDelta (liftIO . genInodeCache tmp)
		maybe noop (restagePointerFile restage f) ic
	go _ = return ()
	
{- Removes the content from a pointer file, replacing it with a pointer.
 -
 - Does not check if the pointer file is modified. -}
depopulatePointerFile :: Key -> FilePath -> Annex ()
depopulatePointerFile key file = do
	mode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus file
	secureErase file
	liftIO $ nukeFile file
	ic <- replaceFile file $ \tmp -> do
		liftIO $ writePointerFile tmp key mode
		withTSDelta (liftIO . genInodeCache tmp)
	maybe noop (restagePointerFile (Restage True) file) ic

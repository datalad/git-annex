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

{- Populates a pointer file with the content of a key. 
 -
 - If the file already has some other content, it is not modified.
 -
 - Returns an InodeCache if it populated the pointer file.
 -}
populatePointerFile :: Restage -> Key -> FilePath -> FilePath -> Annex (Maybe InodeCache)
populatePointerFile restage k obj f = go =<< liftIO (isPointerFile f)
  where
	go (Just k') | k == k' = do
		destmode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus f
		liftIO $ nukeFile f
		(ic, populated) <- replaceFile f $ \tmp -> do
			ok <- linkOrCopy k obj tmp destmode >>= \case
				Just _ -> thawContent tmp >> return True
				Nothing -> liftIO (writePointerFile tmp k destmode) >> return False
			ic <- withTSDelta (liftIO . genInodeCache tmp)
			return (ic, ok)
		maybe noop (restagePointerFile restage f) ic
		if populated
			then return ic
			else return Nothing
	go _ = return Nothing
	
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

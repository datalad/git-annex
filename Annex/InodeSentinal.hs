{- git-annex inode sentinal file
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Annex.InodeSentinal where

import Annex.Common
import qualified Annex
import Utility.InodeCache
import Annex.Perms

{- If the sentinal shows the inodes have changed, only the size and mtime
 - are compared. -}
compareInodeCaches :: InodeCache -> InodeCache -> Annex Bool
compareInodeCaches x y
	| compareStrong x y = return True
	| otherwise = ifM inodesChanged
		( return $ compareWeak x y
		, return False
		)

compareInodeCachesWith :: Annex InodeComparisonType
compareInodeCachesWith = ifM inodesChanged ( return Weakly, return Strongly )

{- Checks if one of the provided old InodeCache matches the current
 - version of a file. -}
sameInodeCache :: OsPath -> [InodeCache] -> Annex Bool
sameInodeCache file [] = do
	fastDebug "Annex.InodeSentinal" $
		fromOsPath file ++ " inode cache empty"
	return False
sameInodeCache file old = go =<< withTSDelta (liftIO . genInodeCache file)
  where
	go Nothing = do
		fastDebug "Annex.InodeSentinal" $
			fromOsPath file ++ " not present, cannot compare with inode cache"
		return False
	go (Just curr) = ifM (elemInodeCaches curr old)
		( return True
		, do
			fastDebug "Annex.InodeSentinal" $
				fromOsPath file ++ " (" ++ show curr ++ ") does not match inode cache (" ++ show old ++ ")"
			return False
		)

elemInodeCaches :: InodeCache -> [InodeCache] -> Annex Bool
elemInodeCaches _ [] = return False
elemInodeCaches c (l:ls) = ifM (compareInodeCaches c l)
	( return True
	, elemInodeCaches c ls
	)

{- Some filesystems get new inodes each time they are mounted.
 - In order to work on such a filesystem, a sentinal file is used to detect
 - when the inodes have changed.
 -
 - If the sentinal file does not exist, we have to assume that the
 - inodes have changed.
 -}
inodesChanged :: Annex Bool
inodesChanged = sentinalInodesChanged <$> sentinalStatus

withTSDelta :: (TSDelta -> Annex a) -> Annex a
withTSDelta a = a =<< getTSDelta

getTSDelta :: Annex TSDelta
#ifdef mingw32_HOST_OS
getTSDelta = sentinalTSDelta <$> sentinalStatus
#else
getTSDelta = pure noTSDelta -- optimisation
#endif

sentinalStatus :: Annex SentinalStatus
sentinalStatus = maybe check return =<< Annex.getState Annex.sentinalstatus
  where
	check = do
		sc <- liftIO . checkSentinalFile =<< annexSentinalFile
		Annex.changeState $ \s -> s { Annex.sentinalstatus = Just sc }
		return sc

{- The sentinal file is only created when first initializing a repository.
 - If there are any annexed objects in the repository already, creating
 - the file would invalidate their inode caches. -}
createInodeSentinalFile :: Bool -> Annex ()
createInodeSentinalFile evenwithobjects = 
	unlessM (alreadyexists <||> hasobjects) $ do
		s <- annexSentinalFile
		createAnnexDirectory (parentDir (sentinalFile s))
		liftIO $ writeSentinalFile s
		setAnnexFilePerm (sentinalFile s)
		setAnnexFilePerm (sentinalCacheFile s)
  where
	alreadyexists = liftIO. sentinalFileExists =<< annexSentinalFile
	hasobjects
		| evenwithobjects = pure False
		| otherwise = liftIO . doesDirectoryExist
			=<< fromRepo gitAnnexObjectDir

annexSentinalFile :: Annex SentinalFile
annexSentinalFile = do
	sentinalfile <- fromRepo gitAnnexInodeSentinal
	sentinalcachefile <- fromRepo gitAnnexInodeSentinalCache
	return SentinalFile
		{ sentinalFile = sentinalfile
		, sentinalCacheFile = sentinalcachefile
		}

{- git-annex inode sentinal file
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.InodeSentinal where

import Annex.Common
import qualified Annex
import Utility.InodeCache
import Annex.Perms

{- If the sendinal shows the inodes have changed, only the size and mtime
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
sameInodeCache :: FilePath -> [InodeCache] -> Annex Bool
sameInodeCache _ [] = return False
sameInodeCache file old = go =<< withTSDelta (liftIO . genInodeCache file)
  where
	go Nothing = return False
	go (Just curr) = elemInodeCaches curr old

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
  where
	alreadyexists = liftIO. sentinalFileExists =<< annexSentinalFile
	hasobjects
		| evenwithobjects = pure False
		| otherwise = liftIO . doesDirectoryExist =<< fromRepo gitAnnexObjectDir

annexSentinalFile :: Annex SentinalFile
annexSentinalFile = do
	sentinalfile <- fromRepo gitAnnexInodeSentinal
	sentinalcachefile <- fromRepo gitAnnexInodeSentinalCache
	return SentinalFile
		{ sentinalFile = sentinalfile
		, sentinalCacheFile = sentinalcachefile
		}

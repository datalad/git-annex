{- git-annex low-level content functions
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content.LowLevel where

import System.PosixCompat.Files

import Annex.Common
import Logs.Transfer
import qualified Annex
import Utility.DiskFree
import Utility.FileMode
import Utility.DataUnits
import Utility.CopyFile

{- Runs the secure erase command if set, otherwise does nothing.
 - File may or may not be deleted at the end; caller is responsible for
 - making sure it's deleted. -}
secureErase :: FilePath -> Annex ()
secureErase file = maybe noop go =<< annexSecureEraseCommand <$> Annex.getGitConfig
  where
	go basecmd = void $ liftIO $
		boolSystem "sh" [Param "-c", Param $ gencmd basecmd]
	gencmd = massReplace [ ("%file", shellEscape file) ]

data LinkedOrCopied = Linked | Copied

{- Hard links or copies src to dest, which must not already exist.
 -
 - Only uses a hard link when annex.thin is enabled and when src is
 - not already hardlinked to elsewhere.
 -
 - Checks disk reserve before copying against the size of the key,
 - and will fail if not enough space, or if the dest file already exists.
 -
 - The FileMode, if provided, influences the mode of the dest file.
 - In particular, if it has an execute bit set, the dest file's
 - execute bit will be set. The mode is not fully copied over because
 - git doesn't support file modes beyond execute.
 -}
linkOrCopy :: Key -> FilePath -> FilePath -> Maybe FileMode -> Annex (Maybe LinkedOrCopied)
linkOrCopy = linkOrCopy' (annexThin <$> Annex.getGitConfig)

linkOrCopy' :: Annex Bool -> Key -> FilePath -> FilePath -> Maybe FileMode -> Annex (Maybe LinkedOrCopied)
linkOrCopy' canhardlink key src dest destmode = catchDefaultIO Nothing $
	ifM canhardlink
		( hardlink
		, copy =<< getstat
		)
  where
	hardlink = do
		s <- getstat
		if linkCount s > 1
			then copy s
			else liftIO (createLink src dest >> preserveGitMode dest destmode >> return (Just Linked))
				`catchIO` const (copy s)
	copy s = ifM (checkedCopyFile' key src dest destmode s)
		( return (Just Copied)
		, return Nothing
		)
	getstat = liftIO $ getFileStatus src

{- Checks disk space before copying. -}
checkedCopyFile :: Key -> FilePath -> FilePath -> Maybe FileMode -> Annex Bool
checkedCopyFile key src dest destmode = catchBoolIO $
	checkedCopyFile' key src dest destmode
		=<< liftIO (getFileStatus src)

checkedCopyFile' :: Key -> FilePath -> FilePath -> Maybe FileMode -> FileStatus -> Annex Bool
checkedCopyFile' key src dest destmode s = catchBoolIO $
	ifM (checkDiskSpace' (fromIntegral $ fileSize s) (Just $ takeDirectory dest) key 0 True)
		( liftIO $
			copyFileExternal CopyAllMetaData src dest
				<&&> preserveGitMode dest destmode
		, return False
		)

preserveGitMode :: FilePath -> Maybe FileMode -> IO Bool
preserveGitMode f (Just mode)
	| isExecutable mode = catchBoolIO $ do
		modifyFileMode f $ addModes executeModes
		return True
	| otherwise = catchBoolIO $ do
		modifyFileMode f $ removeModes executeModes
		return True
preserveGitMode _ _ = return True

{- Checks that there is disk space available to store a given key,
 - in a destination directory (or the annex) printing a warning if not. 
 -
 - If the destination is on the same filesystem as the annex,
 - checks for any other running downloads, removing the amount of data still
 - to be downloaded from the free space. This way, we avoid overcommitting
 - when doing concurrent downloads.
 -}
checkDiskSpace :: Maybe FilePath -> Key -> Integer -> Bool -> Annex Bool
checkDiskSpace destdir key = checkDiskSpace' (fromMaybe 1 (fromKey keySize key)) destdir key

{- Allows specifying the size of the key, if it's known, which is useful
 - as not all keys know their size. -}
checkDiskSpace' :: Integer -> Maybe FilePath -> Key -> Integer -> Bool -> Annex Bool
checkDiskSpace' need destdir key alreadythere samefilesystem = ifM (Annex.getState Annex.force)
	( return True
	, do
		-- We can't get inprogress and free at the same
		-- time, and both can be changing, so there's a
		-- small race here. Err on the side of caution
		-- by getting inprogress first, so if it takes
		-- a while, we'll see any decrease in the free
		-- disk space.
		inprogress <- if samefilesystem
			then sizeOfDownloadsInProgress (/= key)
			else pure 0
		dir >>= liftIO . getDiskFree >>= \case
			Just have -> do
				reserve <- annexDiskReserve <$> Annex.getGitConfig
				let delta = need + reserve - have - alreadythere + inprogress
				let ok = delta <= 0
				unless ok $
					warning $ needMoreDiskSpace delta
				return ok
			_ -> return True
	)
  where
	dir = maybe (fromRawFilePath <$> fromRepo gitAnnexDir) return destdir

needMoreDiskSpace :: Integer -> String
needMoreDiskSpace n = "not enough free space, need " ++ 
	roughSize storageUnits True n ++ " more" ++ forcemsg
  where
	forcemsg = " (use --force to override this check or adjust annex.diskreserve)"

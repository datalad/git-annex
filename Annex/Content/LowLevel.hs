{- git-annex low-level content functions
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content.LowLevel where

import Annex.Common
import Annex.Hook
import Logs.Transfer
import qualified Annex
import Utility.DiskFree
import Utility.FileMode
import Utility.DataUnits
import Utility.CopyFile
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (linkCount)

{- Runs the secure erase command if set, otherwise does nothing.
 - File may or may not be deleted at the end; caller is responsible for
 - making sure it's deleted. -}
secureErase :: OsPath -> Annex ()
secureErase = void . runAnnexPathHook "%file"
	secureEraseAnnexHook annexSecureEraseCommand

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
linkOrCopy :: Key -> OsPath -> OsPath -> Maybe FileMode -> Annex (Maybe LinkedOrCopied)
linkOrCopy = linkOrCopy' (annexThin <$> Annex.getGitConfig)

linkOrCopy' :: Annex Bool -> Key -> OsPath -> OsPath -> Maybe FileMode -> Annex (Maybe LinkedOrCopied)
linkOrCopy' canhardlink key src dest destmode = catchDefaultIO Nothing $
	ifM canhardlink
		( hardlinkorcopy
		, copy =<< getstat
		)
  where
	hardlinkorcopy = do
		s <- getstat
		if linkCount s > 1
			then copy s
			else hardlink `catchIO` const (copy s)
	hardlink = liftIO $ do
		R.createLink (fromOsPath src) (fromOsPath dest)
		void $ preserveGitMode dest destmode
		return (Just Linked)
	copy s = ifM (checkedCopyFile' key src dest destmode s)
		( return (Just Copied)
		, return Nothing
		)
	getstat = liftIO $ R.getFileStatus (fromOsPath src)

{- Checks disk space before copying. -}
checkedCopyFile :: Key -> OsPath -> OsPath -> Maybe FileMode -> Annex Bool
checkedCopyFile key src dest destmode = catchBoolIO $
	checkedCopyFile' key src dest destmode
		=<< liftIO (R.getFileStatus (fromOsPath src))

checkedCopyFile' :: Key -> OsPath -> OsPath -> Maybe FileMode -> FileStatus -> Annex Bool
checkedCopyFile' key src dest destmode s = catchBoolIO $ do
	sz <- liftIO $ getFileSize' src s
	ifM (checkDiskSpace' sz (Just $ takeDirectory dest) key 0 True)
		( liftIO $
			copyFileExternal CopyAllMetaData src dest
				<&&> preserveGitMode dest destmode
		, return False
		)

preserveGitMode :: OsPath -> Maybe FileMode -> IO Bool
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
checkDiskSpace :: Maybe FileSize -> Maybe OsPath -> Key -> Integer -> Bool -> Annex Bool
checkDiskSpace msz destdir key = checkDiskSpace' sz destdir key
  where
	sz = fromMaybe 1 (fromKey keySize key <|> msz)

checkDiskSpace' :: FileSize -> Maybe OsPath -> Key -> Integer -> Bool -> Annex Bool
checkDiskSpace' sz destdir key alreadythere samefilesystem = ifM (Annex.getRead Annex.force)
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
		dir >>= liftIO . getDiskFree . fromOsPath >>= \case
			Just have -> do
				reserve <- annexDiskReserve <$> Annex.getGitConfig
				let delta = sz + reserve - have - alreadythere + inprogress
				let ok = delta <= 0
				unless ok $
					warning $ UnquotedString $ 
						needMoreDiskSpace delta
				return ok
			_ -> return True
	)
  where
	dir = maybe (fromRepo gitAnnexDir) return destdir

needMoreDiskSpace :: Integer -> String
needMoreDiskSpace n = "not enough free space, need " ++ 
	roughSize storageUnits True n ++ " more" ++ forcemsg
  where
	forcemsg = " (use --force to override this check or adjust annex.diskreserve)"

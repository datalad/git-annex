{- git-annex v0 -> v1 upgrade support
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Upgrade.V0 where

import Annex.Common
import Types.Upgrade
import Annex.Content
import qualified Upgrade.V1
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (isRegularFile)

upgrade :: Annex UpgradeResult
upgrade = do
	showAction "v0 to v1"

	-- do the reorganisation of the key files
	olddir <- fromRawFilePath <$> fromRepo gitAnnexDir
	keys <- getKeysPresent0 olddir
	forM_ keys $ \k ->
		moveAnnex k (AssociatedFile Nothing)
			(toRawFilePath $ olddir </> keyFile0 k)

	-- update the symlinks to the key files
	-- No longer needed here; V1.upgrade does the same thing

	-- Few people had v0 repos, so go the long way around from 0 -> 1 -> 2
	Upgrade.V1.upgrade

-- these stayed unchanged between v0 and v1
keyFile0 :: Key -> FilePath
keyFile0 = Upgrade.V1.keyFile1
fileKey0 :: FilePath -> Key
fileKey0 = Upgrade.V1.fileKey1
lookupKey0 :: FilePath -> Annex (Maybe (Key, Backend))
lookupKey0 = Upgrade.V1.lookupKey1

getKeysPresent0 :: FilePath -> Annex [Key]
getKeysPresent0 dir = ifM (liftIO $ doesDirectoryExist dir)
	( liftIO $ map fileKey0
		<$> (filterM present =<< getDirectoryContents dir)
	, return []
	)
  where
	present d = do
		result <- tryIO $
			R.getFileStatus $ toRawFilePath $
				dir ++ "/" ++ takeFileName d
		case result of
			Right s -> return $ isRegularFile s
			Left _ -> return False

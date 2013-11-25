{- git-annex assistant upgrading
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Upgrade where

import Assistant.Common
import Assistant.Restart
import qualified Annex
import Assistant.Alert
import Assistant.DaemonStatus
import Utility.Env
import Types.Distribution
import Logs.Transfer
import Logs.Web
import Logs.Presence
import Logs.Location
import Annex.Content
import qualified Backend
import qualified Types.Backend
import qualified Types.Key
import Assistant.TransferQueue
import Assistant.TransferSlots
import Remote (remoteFromUUID)
import Annex.Path
import Config.Files
import Utility.ThreadScheduler
import Utility.Tmp
import Utility.UserInfo
import qualified Utility.Lsof as Lsof
#ifdef darwin_HOST_OS
import Utility.CopyFile
#endif

import qualified Data.Map as M
import Data.Tuple.Utils

{- Upgrade without interaction in the webapp. -}
unattendedUpgrade :: Assistant ()
unattendedUpgrade = do
	prepUpgrade
	url <- runRestart
	postUpgrade url

prepUpgrade :: Assistant ()
prepUpgrade = do
	void $ addAlert upgradingAlert
	void $ liftIO $ setEnv upgradedEnv "1" True
	prepRestart

postUpgrade :: URLString -> Assistant ()
postUpgrade = postRestart

autoUpgradeEnabled :: Assistant Bool
autoUpgradeEnabled = liftAnnex $ (==) AutoUpgrade . annexAutoUpgrade <$> Annex.getGitConfig

checkSuccessfulUpgrade :: IO Bool
checkSuccessfulUpgrade = isJust <$> getEnv upgradedEnv

upgradedEnv :: String
upgradedEnv = "GIT_ANNEX_UPGRADED"

{- Start downloading the distribution key from the web.
 - Install a hook that will be run once the download is complete,
 - and finishes the upgrade.
 -
 - Creates the destination directory where the upgrade will be installed
 - early, in order to check if another upgrade has happened (or is
 - happending). On failure, the directory is removed.
 -}
startDistributionDownload :: GitAnnexDistribution -> Assistant ()
startDistributionDownload d = go =<< liftIO . newVersionLocation d =<< liftIO oldVersionLocation
  where
  	go Nothing = debug ["Skipping redundant upgrade"]
	go (Just dest) = do
		liftAnnex $ setUrlPresent k u
		hook <- asIO1 $ distributionDownloadComplete d dest cleanup
		modifyDaemonStatus_ $ \s -> s
			{ transferHook = M.insert k hook (transferHook s) }
		maybe noop (queueTransfer "upgrade" Next (Just f) t)
			=<< liftAnnex (remoteFromUUID webUUID)
		startTransfer t
	k = distributionKey d
	u = distributionUrl d
	f = takeFileName u ++ " (for upgrade)"
	t = Transfer
		{ transferDirection = Download
		, transferUUID = webUUID
		, transferKey = k
		}
	cleanup = liftAnnex $ do
		removeAnnex k
		setUrlMissing k u
		logStatus k InfoMissing

{- Called once the download is done.
 - Passed an action that can be used to clean up the downloaded file.
 -
 - Fsck the key to verify the download.
 -}
distributionDownloadComplete :: GitAnnexDistribution -> FilePath -> Assistant () -> Transfer -> Assistant ()
distributionDownloadComplete d dest cleanup t 
	| transferDirection t == Download = do
		debug ["finished downloading git-annex distribution"]
		maybe (failedupgrade "bad download") go
			=<< liftAnnex (withObjectLoc k fsckit (getM fsckit))
	| otherwise = cleanup
  where
	k = distributionKey d
	fsckit f = case Backend.maybeLookupBackendName (Types.Key.keyBackendName k) of
		Nothing -> return $ Just f
		Just b -> case Types.Backend.fsckKey b of
			Nothing -> return $ Just f
			Just a -> ifM (a k f)
				( return $ Just f
				, return Nothing
				)
	go f = do
		ua <- asIO $ upgradeToDistribution dest cleanup f
		fa <- asIO1 failedupgrade
		liftIO $ ua `catchNonAsync` (fa . show)
	failedupgrade msg = do
		void $ addAlert $ upgradeFailedAlert msg
		cleanup
		liftIO $ void $ tryIO $ removeDirectoryRecursive dest

{- The upgrade method varies by OS.
 -
 - In general, find where the distribution was installed before,
 - and unpack the new distribution next to it (in a versioned directory).
 - Then update the programFile to point to the new version.
 -}
upgradeToDistribution :: FilePath -> Assistant () -> FilePath -> Assistant ()
upgradeToDistribution newdir cleanup distributionfile = do
	liftIO $ createDirectoryIfMissing True newdir
	(program, deleteold) <- unpack
	changeprogram program
	cleanup
	prepUpgrade
	url <- runRestart
	{- At this point, the new assistant is fully running, so
	 - it's safe to delete the old version. -}
	liftIO $ void $ tryIO deleteold
	postUpgrade url
  where
	changeprogram program = liftIO $ do
		unlessM (boolSystem program [Param "version"]) $
			error "New git-annex program failed to run! Not using."
		pf <- programFile
		liftIO $ writeFile pf program
	
#ifdef darwin_HOST_OS
	{- OS X uses a dmg, so mount it, and copy the contents into place. -}
	unpack = liftIO $ do
		olddir <- oldVersionLocation
		withTmpDirIn (parentDir newdir) "git-annex.upgrade" $ \tmpdir -> do
			void $ boolSystem "hdiutil"
				[ Param "attach", File distributionfile
				, Param "-mountroot", File tmpdir
				]
			sanitycheck tmpdir
			void $ boolSystem "cp"
				[ Param "-R"
				-- Trailing slash to copy directory contents.
				, File $ tmpdir ++ "/"
				, File newdir
				]
			void $ boolSystem "hdiutil"
				[ Param "eject"
				, Param distributionfile
				]
		let deleteold = do
			deleteFromManifest $ olddir </> "Contents" </> "MacOS"
			makeorigsymlink olddir
		return (newdir </> installBase </> "Contents" </> "MacOS" </> "git-annex", deleteold)
#else
	{- Linux uses a tarball (so could other POSIX systems), so
	 - untar it (into a temp directory) and move the directory
	 - into place. -}
	unpack = liftIO $ do
		olddir <- oldVersionLocation
		withTmpDirIn (parentDir newdir) "git-annex.upgrade" $ \tmpdir -> do
			let tarball = tmpdir </> "tar"
			-- Cannot rely on filename extension, and this also
			-- avoids problems if tar doesn't support transparent
			-- decompression.
			void $ boolSystem "sh"
				[ Param "-c"
				, Param $ "zcat < " ++ shellEscape distributionfile ++
					" > " ++ shellEscape tarball
				]
			tarok <- boolSystem "tar"
				[ Param "xf"
				, Param tarball
				, Param "--directory", File tmpdir
				]
			unless tarok $
				error $ "failed to untar " ++ distributionfile
			sanitycheck tmpdir
			installby rename newdir (tmpdir </> installBase)
		let deleteold = do
			deleteFromManifest olddir
			makeorigsymlink olddir
		return (newdir </> "git-annex", deleteold)
#endif
	sanitycheck dir = 
		unlessM (doesDirectoryExist $ dir </> installBase) $
			error $ "did not find " ++ installBase ++ " in " ++ distributionfile
	installby a dstdir srcdir =
		mapM_ (\x -> a x (dstdir </> takeFileName x))
			=<< dirContents srcdir
	makeorigsymlink olddir = do
		let origdir = parentDir olddir </> installBase
		nukeFile origdir
		createSymbolicLink newdir origdir

{- Finds where the old version was installed. -}
oldVersionLocation :: IO FilePath
oldVersionLocation = do
#ifdef darwin_HOST_OS
	pdir <- parentDir <$> readProgramFile
	let dirs = splitDirectories pdir
	{- It will probably be deep inside a git-annex.app directory. -}
	let p = takeWhile (/= "git-annex.app") dirs
	let olddir = if p == dirs
		then pdir
		else joinPath (p ++ ["git-annex.app"])
#else
	olddir <- parentDir <$> readProgramFile
#endif
	when (null olddir) $
		error $ "Cannot find old distribution bundle; not upgrading."
	return olddir

{- Finds a place to install the new version.
 - Generally, put it in the parent directory of where the old version was
 - installed, and use a version number in the directory name.
 - If unable to write to there, instead put it in the home directory.
 -
 - The directory is created. If it already exists, returns Nothing.
 -}
newVersionLocation :: GitAnnexDistribution -> FilePath -> IO (Maybe FilePath)
newVersionLocation d olddir = 
	trymkdir newloc $ do
		home <- myHomeDir
		trymkdir (home </> s) $
			return Nothing
  where
	s = installBase ++ "." ++ distributionVersion d
	topdir = parentDir olddir
	newloc = topdir </> s
	trymkdir dir fallback =
		(createDirectory dir >> return (Just dir))
			`catchIO` const fallback

installBase :: String
installBase = "git-annex." ++
#ifdef linux_HOST_OS
	"linux"
#else
#ifdef darwin_HOST_OS
	"app"
#else
	"dir"
#endif
#endif

deleteFromManifest :: FilePath -> IO ()
deleteFromManifest dir = do
	fs <- map (dir </>) . lines <$> catchDefaultIO "" (readFile manifest)
	mapM_ nukeFile fs
	nukeFile manifest
	removeEmptyRecursive dir
  where
	manifest = dir </> "git-annex.MANIFEST"

removeEmptyRecursive :: FilePath -> IO ()
removeEmptyRecursive dir = do
	print ("remove", dir)
	mapM_ removeEmptyRecursive =<< dirContents dir
	void $ tryIO $ removeDirectory dir

{- This is a file that the UpgradeWatcher can watch for modifications to
 - detect when git-annex has been upgraded.
 -}
upgradeFlagFile :: IO (Maybe FilePath)
upgradeFlagFile = ifM usingDistribution
	( Just <$> programFile
	, programPath
	)

{- Sanity check to see if an upgrade is complete and the program is ready
 - to be run. -}
upgradeSanityCheck :: IO Bool
upgradeSanityCheck = ifM usingDistribution
	( doesFileExist =<< programFile
	, do
		-- Ensure that the program is present, and has no writers,
		-- and can be run. This should handle distribution
		-- upgrades, manual upgrades, etc.
		v <- programPath
		case v of
			Nothing -> return False
			Just program -> do
				untilM (doesFileExist program <&&> nowriter program) $
					threadDelaySeconds (Seconds 60)
				boolSystem program [Param "version"]
	)
  where
	nowriter f = null
		. filter (`elem` [Lsof.OpenReadWrite, Lsof.OpenWriteOnly])
		. map snd3
		<$> Lsof.query [f]

usingDistribution :: IO Bool
usingDistribution = isJust <$> getEnv "GIT_ANNEX_STANDLONE_ENV"

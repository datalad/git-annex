{- git-annex configuration
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.GitConfig ( 
	Configurable(..),
	GitConfig(..),
	extractGitConfig,
	mergeGitConfig,
	RemoteGitConfig(..),
	extractRemoteGitConfig,
) where

import Common
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import Git.ConfigTypes
import Utility.DataUnits
import Config.Cost
import Types.UUID
import Types.Distribution
import Types.Availability
import Types.NumCopies
import Types.Difference
import Types.RefSpec
import Utility.HumanTime
import Utility.Gpg (GpgCmd, mkGpgCmd)
import Utility.ThreadScheduler (Seconds(..))

-- | A configurable value, that may not be fully determined yet.
data Configurable a
	= HasConfig a
	-- ^ Value is fully determined.
	| DefaultConfig a
	-- ^ A default value is known, but not all config sources
	-- have been read yet.
	deriving (Show)

{- Main git-annex settings. Each setting corresponds to a git-config key
 - such as annex.foo -}
data GitConfig = GitConfig
	{ annexVersion :: Maybe String
	, annexUUID :: UUID
	, annexNumCopies :: Maybe NumCopies
	, annexDiskReserve :: Integer
	, annexDirect :: Bool
	, annexBackends :: [String]
	, annexQueueSize :: Maybe Int
	, annexBloomCapacity :: Maybe Int
	, annexBloomAccuracy :: Maybe Int
	, annexSshCaching :: Maybe Bool
	, annexAlwaysCommit :: Bool
	, annexDelayAdd :: Maybe Int
	, annexHttpHeaders :: [String]
	, annexHttpHeadersCommand :: Maybe String
	, annexAutoCommit :: Configurable Bool
	, annexSyncContent :: Configurable Bool
	, annexDebug :: Bool
	, annexWebOptions :: [String]
	, annexQuviOptions :: [String]
	, annexAriaTorrentOptions :: [String]
	, annexWebDownloadCommand :: Maybe String
	, annexCrippledFileSystem :: Bool
	, annexLargeFiles :: Maybe String
	, annexAddSmallFiles :: Bool
	, annexFsckNudge :: Bool
	, annexAutoUpgrade :: AutoUpgrade
	, annexExpireUnused :: Maybe (Maybe Duration)
	, annexSecureEraseCommand :: Maybe String
	, annexGenMetaData :: Bool
	, annexListen :: Maybe String
	, annexStartupScan :: Bool
	, annexHardLink :: Bool
	, annexThin :: Bool
	, annexDifferences :: Differences
	, annexUsedRefSpec :: Maybe RefSpec
	, annexVerify :: Bool
	, annexPidLock :: Bool
	, annexPidLockTimeout :: Seconds
	, annexAddUnlocked :: Bool
	, coreSymlinks :: Bool
	, coreSharedRepository :: SharedRepository
	, receiveDenyCurrentBranch :: DenyCurrentBranch
	, gcryptId :: Maybe String
	, gpgCmd :: GpgCmd
	}

extractGitConfig :: Git.Repo -> GitConfig
extractGitConfig r = GitConfig
	{ annexVersion = notempty $ getmaybe (annex "version")
	, annexUUID = maybe NoUUID toUUID $ getmaybe (annex "uuid")
	, annexNumCopies = NumCopies <$> getmayberead (annex "numcopies")
	, annexDiskReserve = fromMaybe onemegabyte $
		readSize dataUnits =<< getmaybe (annex "diskreserve")
	, annexDirect = getbool (annex "direct") False
	, annexBackends = getwords (annex "backends")
	, annexQueueSize = getmayberead (annex "queuesize")
	, annexBloomCapacity = getmayberead (annex "bloomcapacity")
	, annexBloomAccuracy = getmayberead (annex "bloomaccuracy")
	, annexSshCaching = getmaybebool (annex "sshcaching")
	, annexAlwaysCommit = getbool (annex "alwayscommit") True
	, annexDelayAdd = getmayberead (annex "delayadd")
	, annexHttpHeaders = getlist (annex "http-headers")
	, annexHttpHeadersCommand = getmaybe (annex "http-headers-command")
	, annexAutoCommit = configurable True $ 
		getmaybebool (annex "autocommit")
	, annexSyncContent = configurable False $ 
		getmaybebool (annex "synccontent")
	, annexDebug = getbool (annex "debug") False
	, annexWebOptions = getwords (annex "web-options")
	, annexQuviOptions = getwords (annex "quvi-options")
	, annexAriaTorrentOptions = getwords (annex "aria-torrent-options")
	, annexWebDownloadCommand = getmaybe (annex "web-download-command")
	, annexCrippledFileSystem = getbool (annex "crippledfilesystem") False
	, annexLargeFiles = getmaybe (annex "largefiles")
	, annexAddSmallFiles = getbool (annex "addsmallfiles") True
	, annexFsckNudge = getbool (annex "fscknudge") True
	, annexAutoUpgrade = toAutoUpgrade $ getmaybe (annex "autoupgrade")
	, annexExpireUnused = maybe Nothing Just . parseDuration
		<$> getmaybe (annex "expireunused")
	, annexSecureEraseCommand = getmaybe (annex "secure-erase-command")
	, annexGenMetaData = getbool (annex "genmetadata") False
	, annexListen = getmaybe (annex "listen")
	, annexStartupScan = getbool (annex "startupscan") True
	, annexHardLink = getbool (annex "hardlink") False
	, annexThin = getbool (annex "thin") False
	, annexDifferences = getDifferences r
	, annexUsedRefSpec = either (const Nothing) Just . parseRefSpec 
		=<< getmaybe (annex "used-refspec")
	, annexVerify = getbool (annex "verify") True
	, annexPidLock = getbool (annex "pidlock") False
	, annexPidLockTimeout = Seconds $ fromMaybe 300 $
		getmayberead (annex "pidlocktimeout")
	, annexAddUnlocked = getbool (annex "addunlocked") False
	, coreSymlinks = getbool "core.symlinks" True
	, coreSharedRepository = getSharedRepository r
	, receiveDenyCurrentBranch = getDenyCurrentBranch r
	, gcryptId = getmaybe "core.gcrypt-id"
	, gpgCmd = mkGpgCmd (getmaybe "gpg.program")
	}
  where
	getbool k d = fromMaybe d $ getmaybebool k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	getmayberead k = readish =<< getmaybe k
	getmaybe k = Git.Config.getMaybe k r
	getlist k = Git.Config.getList k r
	getwords k = fromMaybe [] $ words <$> getmaybe k

	configurable d Nothing = DefaultConfig d
	configurable _ (Just v) = HasConfig v

	annex k = "annex." ++ k
			
	onemegabyte = 1000000

{- Merge a GitConfig that comes from git-config with one containing
 - repository-global defaults. -}
mergeGitConfig :: GitConfig -> GitConfig -> GitConfig
mergeGitConfig gitconfig repoglobals = gitconfig
	{ annexAutoCommit = merge annexAutoCommit
	, annexSyncContent = merge annexSyncContent
	}
  where
	merge f = case f gitconfig of
		HasConfig v -> HasConfig v
		DefaultConfig d -> case f repoglobals of
			HasConfig v -> HasConfig v
			DefaultConfig _ -> HasConfig d

{- Per-remote git-annex settings. Each setting corresponds to a git-config
 - key such as <remote>.annex-foo, or if that is not set, a default from
 - annex.foo -}
data RemoteGitConfig = RemoteGitConfig
	{ remoteAnnexCost :: Maybe Cost
	, remoteAnnexCostCommand :: Maybe String
	, remoteAnnexIgnore :: Bool
	, remoteAnnexSync :: Bool
	, remoteAnnexReadOnly :: Bool
	, remoteAnnexVerify :: Bool
	, remoteAnnexTrustLevel :: Maybe String
	, remoteAnnexStartCommand :: Maybe String
	, remoteAnnexStopCommand :: Maybe String
	, remoteAnnexAvailability :: Maybe Availability
	, remoteAnnexBare :: Maybe Bool

	{- These settings are specific to particular types of remotes
	 - including special remotes. -}
	, remoteAnnexShell :: Maybe String
	, remoteAnnexSshOptions :: [String]
	, remoteAnnexRsyncOptions :: [String]
	, remoteAnnexRsyncUploadOptions :: [String]
	, remoteAnnexRsyncDownloadOptions :: [String]
	, remoteAnnexRsyncTransport :: [String]
	, remoteAnnexGnupgOptions :: [String]
	, remoteAnnexGnupgDecryptOptions :: [String]
	, remoteAnnexRsyncUrl :: Maybe String
	, remoteAnnexBupRepo :: Maybe String
	, remoteAnnexTahoe :: Maybe FilePath
	, remoteAnnexBupSplitOptions :: [String]
	, remoteAnnexDirectory :: Maybe FilePath
	, remoteAnnexGCrypt :: Maybe String
	, remoteAnnexDdarRepo :: Maybe String
	, remoteAnnexHookType :: Maybe String
	, remoteAnnexExternalType :: Maybe String
	{- A regular git remote's git repository config. -}
	, remoteGitConfig :: Maybe GitConfig
	}

extractRemoteGitConfig :: Git.Repo -> String -> RemoteGitConfig
extractRemoteGitConfig r remotename = RemoteGitConfig
	{ remoteAnnexCost = getmayberead "cost"
	, remoteAnnexCostCommand = notempty $ getmaybe "cost-command"
	, remoteAnnexIgnore = getbool "ignore" False
	, remoteAnnexSync = getbool "sync" True
	, remoteAnnexReadOnly = getbool "readonly" False
	, remoteAnnexVerify = getbool "verify" True
	, remoteAnnexTrustLevel = notempty $ getmaybe "trustlevel"
	, remoteAnnexStartCommand = notempty $ getmaybe "start-command"
	, remoteAnnexStopCommand = notempty $ getmaybe "stop-command"
	, remoteAnnexAvailability = getmayberead "availability"
	, remoteAnnexBare = getmaybebool "bare"

	, remoteAnnexShell = getmaybe "shell"
	, remoteAnnexSshOptions = getoptions "ssh-options"
	, remoteAnnexRsyncOptions = getoptions "rsync-options"
	, remoteAnnexRsyncDownloadOptions = getoptions "rsync-download-options"
	, remoteAnnexRsyncUploadOptions = getoptions "rsync-upload-options"
	, remoteAnnexRsyncTransport = getoptions "rsync-transport"
	, remoteAnnexGnupgOptions = getoptions "gnupg-options"
	, remoteAnnexGnupgDecryptOptions = getoptions "gnupg-decrypt-options"
	, remoteAnnexRsyncUrl = notempty $ getmaybe "rsyncurl"
	, remoteAnnexBupRepo = getmaybe "buprepo"
	, remoteAnnexTahoe = getmaybe "tahoe"
	, remoteAnnexBupSplitOptions = getoptions "bup-split-options"
	, remoteAnnexDirectory = notempty $ getmaybe "directory"
	, remoteAnnexGCrypt = notempty $ getmaybe "gcrypt"
	, remoteAnnexDdarRepo = getmaybe "ddarrepo"
	, remoteAnnexHookType = notempty $ getmaybe "hooktype"
	, remoteAnnexExternalType = notempty $ getmaybe "externaltype"
	, remoteGitConfig = Nothing
	}
  where
	getbool k d = fromMaybe d $ getmaybebool k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	getmayberead k = readish =<< getmaybe k
	getmaybe k = mplus (Git.Config.getMaybe (key k) r)
		(Git.Config.getMaybe (remotekey k) r)
	getoptions k = fromMaybe [] $ words <$> getmaybe k

	key k = "annex." ++ k
	remotekey k = "remote." ++ remotename ++ ".annex-" ++ k

notempty :: Maybe String -> Maybe String	
notempty Nothing = Nothing
notempty (Just "") = Nothing
notempty (Just s) = Just s

instance Default RemoteGitConfig where
	def = extractRemoteGitConfig Git.Construct.fromUnknown "dummy"

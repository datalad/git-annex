{- git-annex configuration
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.GitConfig ( 
	Configurable(..),
	ConfigSource(..),
	GitConfig(..),
	extractGitConfig,
	mergeGitConfig,
	globalConfigs,
	RemoteGitConfig(..),
	extractRemoteGitConfig,
	dummyRemoteGitConfig,
	annexConfig,
	RemoteNameable(..),
	remoteAnnexConfig,
	remoteConfig,
) where

import Common
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import Git.Types
import Git.ConfigTypes
import Git.Branch (CommitMode(..))
import Utility.DataUnits
import Config.Cost
import Types.UUID
import Types.Distribution
import Types.Availability
import Types.Concurrency
import Types.NumCopies
import Types.Difference
import Types.RefSpec
import Types.RepoVersion
import Config.DynamicConfig
import Utility.HumanTime
import Utility.Gpg (GpgCmd, mkGpgCmd)
import Utility.ThreadScheduler (Seconds(..))
import Utility.Url (Scheme, mkScheme)

import Control.Concurrent.STM
import qualified Data.Set as S
import qualified Data.ByteString as B

-- | A configurable value, that may not be fully determined yet because
-- the global git config has not yet been loaded.
data Configurable a
	= HasGitConfig a
	-- ^ The git config has a value.
	| HasGlobalConfig a
	-- ^ The global config has a value (and the git config does not).
	| DefaultConfig a
	-- ^ A default value is known, but not all config sources
	-- have been read yet.
	deriving (Show)

data ConfigSource = FromGitConfig | FromGlobalConfig

{- Main git-annex settings. Each setting corresponds to a git-config key
 - such as annex.foo -}
data GitConfig = GitConfig
	{ annexVersion :: Maybe RepoVersion
	, annexUUID :: UUID
	, annexNumCopies :: Maybe NumCopies
	, annexDiskReserve :: Integer
	, annexDirect :: Bool
	, annexBackend :: Maybe String
	, annexQueueSize :: Maybe Int
	, annexBloomCapacity :: Maybe Int
	, annexBloomAccuracy :: Maybe Int
	, annexSshCaching :: Maybe Bool
	, annexAlwaysCommit :: Bool
	, annexCommitMessage :: Maybe String
	, annexMergeAnnexBranches :: Bool
	, annexDelayAdd :: Maybe Int
	, annexHttpHeaders :: [String]
	, annexHttpHeadersCommand :: Maybe String
	, annexAutoCommit :: Configurable Bool
	, annexResolveMerge :: Configurable Bool
	, annexSyncContent :: Configurable Bool
	, annexSyncOnlyAnnex :: Configurable Bool
	, annexDebug :: Bool
	, annexWebOptions :: [String]
	, annexYoutubeDlOptions :: [String]
	, annexAriaTorrentOptions :: [String]
	, annexCrippledFileSystem :: Bool
	, annexLargeFiles :: Configurable (Maybe String)
	, annexDotFiles :: Configurable Bool
	, annexGitAddToAnnex :: Bool
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
	, annexAddUnlocked :: Configurable (Maybe String)
	, annexSecureHashesOnly :: Bool
	, annexRetry :: Maybe Integer
	, annexForwardRetry :: Maybe Integer
	, annexRetryDelay :: Maybe Seconds
	, annexAllowedUrlSchemes :: S.Set Scheme
	, annexAllowedIPAddresses :: String
	, annexAllowUnverifiedDownloads :: Bool
	, annexMaxExtensionLength :: Maybe Int
	, annexJobs :: Concurrency
	, annexCacheCreds :: Bool
	, annexAutoUpgradeRepository :: Bool
	, annexCommitMode :: CommitMode
	, annexSkipUnknown :: Bool
	, coreSymlinks :: Bool
	, coreSharedRepository :: SharedRepository
	, receiveDenyCurrentBranch :: DenyCurrentBranch
	, gcryptId :: Maybe String
	, gpgCmd :: GpgCmd
	, mergeDirectoryRenames :: Maybe String
	}

extractGitConfig :: ConfigSource -> Git.Repo -> GitConfig
extractGitConfig configsource r = GitConfig
	{ annexVersion = RepoVersion <$> getmayberead (annexConfig "version")
	, annexUUID = maybe NoUUID toUUID $ getmaybe (annexConfig "uuid")
	, annexNumCopies = NumCopies <$> getmayberead (annexConfig "numcopies")
	, annexDiskReserve = fromMaybe onemegabyte $
		readSize dataUnits =<< getmaybe (annexConfig "diskreserve")
	, annexDirect = getbool (annexConfig "direct") False
	, annexBackend = maybe
		-- annex.backends is the old name of the option, still used
		-- when annex.backend is not set.
		(headMaybe $ getwords (annexConfig "backends"))
		Just
		(getmaybe (annexConfig "backend"))
	, annexQueueSize = getmayberead (annexConfig "queuesize")
	, annexBloomCapacity = getmayberead (annexConfig "bloomcapacity")
	, annexBloomAccuracy = getmayberead (annexConfig "bloomaccuracy")
	, annexSshCaching = getmaybebool (annexConfig "sshcaching")
	, annexAlwaysCommit = getbool (annexConfig "alwayscommit") True
	, annexCommitMessage = getmaybe (annexConfig "commitmessage")
	, annexMergeAnnexBranches = getbool (annexConfig "merge-annex-branches") True
	, annexDelayAdd = getmayberead (annexConfig "delayadd")
	, annexHttpHeaders = getlist (annexConfig "http-headers")
	, annexHttpHeadersCommand = getmaybe (annexConfig "http-headers-command")
	, annexAutoCommit = configurable True $ 
		getmaybebool (annexConfig "autocommit")
	, annexResolveMerge = configurable True $ 
		getmaybebool (annexConfig "resolvemerge")
	, annexSyncContent = configurable False $ 
		getmaybebool (annexConfig "synccontent")
	, annexSyncOnlyAnnex = configurable False $ 
		getmaybebool (annexConfig "synconlyannex")
	, annexDebug = getbool (annexConfig "debug") False
	, annexWebOptions = getwords (annexConfig "web-options")
	, annexYoutubeDlOptions = getwords (annexConfig "youtube-dl-options")
	, annexAriaTorrentOptions = getwords (annexConfig "aria-torrent-options")
	, annexCrippledFileSystem = getbool (annexConfig "crippledfilesystem") False
	, annexLargeFiles = configurable Nothing $
		fmap Just $ getmaybe (annexConfig "largefiles")
	, annexDotFiles = configurable False $
		getmaybebool (annexConfig "dotfiles")
	, annexGitAddToAnnex = getbool (annexConfig "gitaddtoannex") True
	, annexAddSmallFiles = getbool (annexConfig "addsmallfiles") True
	, annexFsckNudge = getbool (annexConfig "fscknudge") True
	, annexAutoUpgrade = toAutoUpgrade $
		getmaybe (annexConfig "autoupgrade")
	, annexExpireUnused = either (const Nothing) Just . parseDuration
		<$> getmaybe (annexConfig "expireunused")
	, annexSecureEraseCommand = getmaybe (annexConfig "secure-erase-command")
	, annexGenMetaData = getbool (annexConfig "genmetadata") False
	, annexListen = getmaybe (annexConfig "listen")
	, annexStartupScan = getbool (annexConfig "startupscan") True
	, annexHardLink = getbool (annexConfig "hardlink") False
	, annexThin = getbool (annexConfig "thin") False
	, annexDifferences = getDifferences r
	, annexUsedRefSpec = either (const Nothing) Just . parseRefSpec 
		=<< getmaybe (annexConfig "used-refspec")
	, annexVerify = getbool (annexConfig "verify") True
	, annexPidLock = getbool (annexConfig "pidlock") False
	, annexPidLockTimeout = Seconds $ fromMaybe 300 $
		getmayberead (annexConfig "pidlocktimeout")
	, annexAddUnlocked = configurable Nothing $
		fmap Just $ getmaybe (annexConfig "addunlocked")
	, annexSecureHashesOnly = getbool (annexConfig "securehashesonly") False
	, annexRetry = getmayberead (annexConfig "retry")
	, annexForwardRetry = getmayberead (annexConfig "forward-retry")
	, annexRetryDelay = Seconds
		<$> getmayberead (annexConfig "retrydelay")
	, annexAllowedUrlSchemes = S.fromList $ map mkScheme $
		maybe ["http", "https", "ftp"] words $
			getmaybe (annexConfig "security.allowed-url-schemes")
	, annexAllowedIPAddresses = fromMaybe "" $
		getmaybe (annexConfig "security.allowed-ip-addresses")
			<|>
		getmaybe (annexConfig "security.allowed-http-addresses") -- old name
	, annexAllowUnverifiedDownloads = (== Just "ACKTHPPT") $
		getmaybe (annexConfig "security.allow-unverified-downloads")
	, annexMaxExtensionLength = getmayberead (annexConfig "maxextensionlength")
	, annexJobs = fromMaybe NonConcurrent $ 
		parseConcurrency =<< getmaybe (annexConfig "jobs")
	, annexCacheCreds = getbool (annexConfig "cachecreds") True
	, annexAutoUpgradeRepository = getbool (annexConfig "autoupgraderepository") True
	, annexCommitMode = if getbool (annexConfig "allowsign") False
		then ManualCommit
		else AutomaticCommit
	, annexSkipUnknown = getbool (annexConfig "skipunknown") True
	, coreSymlinks = getbool "core.symlinks" True
	, coreSharedRepository = getSharedRepository r
	, receiveDenyCurrentBranch = getDenyCurrentBranch r
	, gcryptId = getmaybe "core.gcrypt-id"
	, gpgCmd = mkGpgCmd (getmaybe "gpg.program")
	, mergeDirectoryRenames = getmaybe "directoryrenames"
	}
  where
	getbool k d = fromMaybe d $ getmaybebool k
	getmaybebool k = Git.Config.isTrueFalse' =<< getmaybe' k
	getmayberead k = readish =<< getmaybe k
	getmaybe = fmap fromConfigValue . getmaybe'
	getmaybe' k = Git.Config.getMaybe k r
	getlist k = map fromConfigValue $ Git.Config.getList k r
	getwords k = fromMaybe [] $ words <$> getmaybe k

	configurable d Nothing = DefaultConfig d
	configurable _ (Just v) = case configsource of
		FromGitConfig -> HasGitConfig v
		FromGlobalConfig -> HasGlobalConfig v

	onemegabyte = 1000000

{- Merge a GitConfig that comes from git-config with one containing
 - repository-global defaults. -}
mergeGitConfig :: GitConfig -> GitConfig -> GitConfig
mergeGitConfig gitconfig repoglobals = gitconfig
	{ annexAutoCommit = merge annexAutoCommit
	, annexSyncContent = merge annexSyncContent
	, annexSyncOnlyAnnex = merge annexSyncOnlyAnnex
	, annexResolveMerge = merge annexResolveMerge
	, annexLargeFiles = merge annexLargeFiles
	, annexDotFiles = merge annexDotFiles
	, annexAddUnlocked = merge annexAddUnlocked
	}
  where
	merge f = case f gitconfig of
		HasGitConfig v -> HasGitConfig v
		DefaultConfig d -> case f repoglobals of
			HasGlobalConfig v -> HasGlobalConfig v
			_ -> HasGitConfig d
		HasGlobalConfig v -> HasGlobalConfig v

{- Configs that can be set repository-global. -}
globalConfigs :: [ConfigKey]
globalConfigs =
	[ annexConfig "autocommit"
	, annexConfig "synccontent"
	, annexConfig "synconlyannex"
	, annexConfig "resolvemerge"
	, annexConfig "largefiles"
	, annexConfig "dotfiles"
	, annexConfig "addunlocked"
	]

{- Per-remote git-annex settings. Each setting corresponds to a git-config
 - key such as <remote>.annex-foo, or if that is not set, a default from
 - annex.foo.
 -
 - Note that this is from the perspective of the local repository,
 - it is not influenced in any way by the contents of the remote
 - repository's git config.
 -}
data RemoteGitConfig = RemoteGitConfig
	{ remoteAnnexCost :: DynamicConfig (Maybe Cost)
	, remoteAnnexIgnore :: DynamicConfig Bool
	, remoteAnnexSync :: DynamicConfig Bool
	, remoteAnnexPull :: Bool
	, remoteAnnexPush :: Bool
	, remoteAnnexReadOnly :: Bool
	, remoteAnnexVerify :: Bool
	, remoteAnnexCheckUUID :: Bool
	, remoteAnnexTrackingBranch :: Maybe Git.Ref
	, remoteAnnexTrustLevel :: Maybe String
	, remoteAnnexStartCommand :: Maybe String
	, remoteAnnexStopCommand :: Maybe String
	, remoteAnnexAvailability :: Maybe Availability
	, remoteAnnexSpeculatePresent :: Bool
	, remoteAnnexBare :: Maybe Bool
	, remoteAnnexRetry :: Maybe Integer
	, remoteAnnexForwardRetry :: Maybe Integer
	, remoteAnnexRetryDelay :: Maybe Seconds
	, remoteAnnexAllowUnverifiedDownloads :: Bool
	, remoteAnnexConfigUUID :: Maybe UUID

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
	, remoteAnnexAndroidDirectory :: Maybe FilePath
	, remoteAnnexAndroidSerial :: Maybe String
	, remoteAnnexGCrypt :: Maybe String
	, remoteAnnexGitLFS :: Bool
	, remoteAnnexDdarRepo :: Maybe String
	, remoteAnnexHookType :: Maybe String
	, remoteAnnexExternalType :: Maybe String
	}

{- The Git.Repo is the local repository, which has the remote with the
 - given RemoteName. -}
extractRemoteGitConfig :: Git.Repo -> RemoteName -> STM RemoteGitConfig
extractRemoteGitConfig r remotename = do
	annexcost <- mkDynamicConfig readCommandRunner
		(notempty $ getmaybe "cost-command")
		(getmayberead "cost")
	annexignore <- mkDynamicConfig unsuccessfullCommandRunner
		(notempty $ getmaybe "ignore-command")
		(getbool "ignore" False)
	annexsync <- mkDynamicConfig successfullCommandRunner
		(notempty $ getmaybe "sync-command")
		(getbool "sync" True)
	return $ RemoteGitConfig
		{ remoteAnnexCost = annexcost
		, remoteAnnexIgnore = annexignore
		, remoteAnnexSync = annexsync
		, remoteAnnexPull = getbool "pull" True
		, remoteAnnexPush = getbool "push" True
		, remoteAnnexReadOnly = getbool "readonly" False
		, remoteAnnexCheckUUID = getbool "checkuuid" True
		, remoteAnnexVerify = getbool "verify" True
		, remoteAnnexTrackingBranch = Git.Ref . encodeBS <$>
			( notempty (getmaybe "tracking-branch")
			<|> notempty (getmaybe "export-tracking") -- old name
			)
		, remoteAnnexTrustLevel = notempty $ getmaybe "trustlevel"
		, remoteAnnexStartCommand = notempty $ getmaybe "start-command"
		, remoteAnnexStopCommand = notempty $ getmaybe "stop-command"
		, remoteAnnexAvailability = getmayberead "availability"
		, remoteAnnexSpeculatePresent = getbool "speculate-present" False
		, remoteAnnexBare = getmaybebool "bare"
		, remoteAnnexRetry = getmayberead "retry"
		, remoteAnnexForwardRetry = getmayberead "forward-retry"
		, remoteAnnexRetryDelay = Seconds
			<$> getmayberead "retrydelay"
		, remoteAnnexAllowUnverifiedDownloads = (== Just "ACKTHPPT") $
			getmaybe ("security-allow-unverified-downloads")
		, remoteAnnexConfigUUID = toUUID <$> getmaybe "config-uuid"
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
		, remoteAnnexAndroidDirectory = notempty $ getmaybe "androiddirectory"
		, remoteAnnexAndroidSerial = notempty $ getmaybe "androidserial"
		, remoteAnnexGCrypt = notempty $ getmaybe "gcrypt"
		, remoteAnnexGitLFS = getbool "git-lfs" False
		, remoteAnnexDdarRepo = getmaybe "ddarrepo"
		, remoteAnnexHookType = notempty $ getmaybe "hooktype"
		, remoteAnnexExternalType = notempty $ getmaybe "externaltype"
		}
  where
	getbool k d = fromMaybe d $ getmaybebool k
	getmaybebool k = Git.Config.isTrueFalse' =<< getmaybe' k
	getmayberead k = readish =<< getmaybe k
	getmaybe = fmap fromConfigValue . getmaybe'
	getmaybe' k = mplus (Git.Config.getMaybe (annexConfig k) r)
		(Git.Config.getMaybe (remoteAnnexConfig remotename k) r)
	getoptions k = fromMaybe [] $ words <$> getmaybe k

notempty :: Maybe String -> Maybe String	
notempty Nothing = Nothing
notempty (Just "") = Nothing
notempty (Just s) = Just s

dummyRemoteGitConfig :: IO RemoteGitConfig
dummyRemoteGitConfig = atomically $ 
	extractRemoteGitConfig Git.Construct.fromUnknown "dummy"

type UnqualifiedConfigKey = B.ByteString

{- A global annex setting in git config. -}
annexConfig :: UnqualifiedConfigKey -> ConfigKey
annexConfig key = ConfigKey ("annex." <> key)

class RemoteNameable r where
	getRemoteName :: r -> RemoteName

instance RemoteNameable Git.Repo where
	getRemoteName r = fromMaybe "" (Git.remoteName r)

instance RemoteNameable RemoteName where
	 getRemoteName = id

{- A per-remote annex setting in git config. -}
remoteAnnexConfig :: RemoteNameable r => r -> UnqualifiedConfigKey -> ConfigKey
remoteAnnexConfig r key = remoteConfig r ("annex-" <> key)

{- A per-remote setting in git config. -}
remoteConfig :: RemoteNameable r => r -> UnqualifiedConfigKey -> ConfigKey
remoteConfig r key = ConfigKey $
	"remote." <> encodeBS' (getRemoteName r) <> "." <> key

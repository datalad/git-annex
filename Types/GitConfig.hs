{- git-annex configuration
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.GitConfig ( 
	GlobalConfigurable(..),
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
	RemoteGitConfigField(..),
	remoteGitConfigKey,
	proxyInheritedFields,
) where

import Common
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import Git.Types
import Git.ConfigTypes
import Git.Remote (isRemoteKey, isLegalName, remoteKeyToRemoteName)
import Git.Branch (CommitMode(..))
import Git.Quote (QuotePath(..))
import Utility.DataUnits
import Config.Cost
import Types.UUID
import Types.Distribution
import Types.Concurrency
import Types.NumCopies
import Types.Difference
import Types.RefSpec
import Types.RepoVersion
import Types.StallDetection
import Types.View
import Types.Cluster
import Config.DynamicConfig
import Utility.HumanTime
import Utility.Gpg (GpgCmd, mkGpgCmd)
import Utility.StatelessOpenPGP (SOPCmd(..), SOPProfile(..))
import Utility.ThreadScheduler (Seconds(..))
import Utility.Url (Scheme, mkScheme)
import Network.Socket (PortNumber)
import P2P.Http.Url

import Control.Concurrent.STM
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified System.FilePath.ByteString as P

-- | A configurable value, that may not be fully determined yet because
-- the global git config has not yet been loaded.
data GlobalConfigurable a
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
	, annexAlwaysCompact :: Bool
	, annexCommitMessage :: Maybe String
	, annexCommitMessageCommand :: Maybe String
	, annexMergeAnnexBranches :: Bool
	, annexDelayAdd :: Maybe Int
	, annexHttpHeaders :: [String]
	, annexHttpHeadersCommand :: Maybe String
	, annexAutoCommit :: GlobalConfigurable Bool
	, annexResolveMerge :: GlobalConfigurable Bool
	, annexSyncContent :: GlobalConfigurable (Maybe Bool)
	, annexSyncOnlyAnnex :: GlobalConfigurable Bool
	, annexSyncMigrations :: Bool
	, annexDebug :: Bool
	, annexDebugFilter :: Maybe String
	, annexWebOptions :: [String]
	, annexYoutubeDlOptions :: [String]
	, annexYoutubeDlCommand :: Maybe String
	, annexAriaTorrentOptions :: [String]
	, annexCrippledFileSystem :: Bool
	, annexLargeFiles :: GlobalConfigurable (Maybe String)
	, annexDotFiles :: GlobalConfigurable Bool
	, annexGitAddToAnnex :: Bool
	, annexAddSmallFiles :: Bool
	, annexFsckNudge :: Bool
	, annexAutoUpgrade :: AutoUpgrade
	, annexExpireUnused :: Maybe (Maybe Duration)
	, annexFreezeContentCommand :: Maybe String
	, annexThawContentCommand :: Maybe String
	, annexSecureEraseCommand :: Maybe String
	, annexGenMetaData :: Bool
	, annexListen :: Maybe String
	, annexPort :: Maybe PortNumber
	, annexStartupScan :: Bool
	, annexHardLink :: Bool
	, annexThin :: Bool
	, annexDifferences :: Differences
	, annexUsedRefSpec :: Maybe RefSpec
	, annexVerify :: Bool
	, annexPidLock :: Bool
	, annexPidLockTimeout :: Seconds
	, annexDbDir :: Maybe RawFilePath
	, annexAddUnlocked :: GlobalConfigurable (Maybe String)
	, annexSecureHashesOnly :: Bool
	, annexRetry :: Maybe Integer
	, annexForwardRetry :: Maybe Integer
	, annexRetryDelay :: Maybe Seconds
	, annexAllowedUrlSchemes :: S.Set Scheme
	, annexAllowedIPAddresses :: String
	, annexAllowUnverifiedDownloads :: Bool
	, annexMaxExtensionLength :: Maybe Int
	, annexMaxExtensions :: Maybe Int
	, annexJobs :: Concurrency
	, annexCacheCreds :: Bool
	, annexAutoUpgradeRepository :: Bool
	, annexCommitMode :: CommitMode
	, annexSkipUnknown :: Bool
	, annexAdjustedBranchRefresh :: Integer
	, annexSupportUnlocked :: Bool
	, coreSymlinks :: Bool
	, coreSharedRepository :: SharedRepository
	, coreQuotePath :: QuotePath
	, receiveDenyCurrentBranch :: DenyCurrentBranch
	, gcryptId :: Maybe String
	, gpgCmd :: GpgCmd
	, mergeDirectoryRenames :: Maybe String
	, annexPrivateRepos :: S.Set UUID
	, annexAdviceNoSshCaching :: Bool
	, annexViewUnsetDirectory :: ViewUnset
	, annexClusters :: M.Map RemoteName ClusterUUID
	}

extractGitConfig :: ConfigSource -> Git.Repo -> GitConfig
extractGitConfig configsource r = GitConfig
	{ annexVersion = RepoVersion <$> getmayberead (annexConfig "version")
	, annexUUID = hereuuid
	, annexNumCopies = configuredNumCopies
		<$> getmayberead (annexConfig "numcopies")
	, annexDiskReserve = fromMaybe (onemegabyte * 100) $
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
	, annexAlwaysCompact = getbool (annexConfig "alwayscompact") True
	, annexCommitMessage = getmaybe (annexConfig "commitmessage")
	, annexCommitMessageCommand = getmaybe (annexConfig "commitmessage-command")
	, annexMergeAnnexBranches = getbool (annexConfig "merge-annex-branches") True
	, annexDelayAdd = getmayberead (annexConfig "delayadd")
	, annexHttpHeaders = getlist (annexConfig "http-headers")
	, annexHttpHeadersCommand = getmaybe (annexConfig "http-headers-command")
	, annexAutoCommit = configurable True $ 
		getmaybebool (annexConfig "autocommit")
	, annexResolveMerge = configurable True $ 
		getmaybebool (annexConfig "resolvemerge")
	, annexSyncContent = configurablemaybe $ 
		getmaybebool (annexConfig "synccontent")
	, annexSyncOnlyAnnex = configurable False $ 
		getmaybebool (annexConfig "synconlyannex")
	, annexSyncMigrations = getbool (annexConfig "syncmigrations") True
	, annexDebug = getbool (annexConfig "debug") False
	, annexDebugFilter = getmaybe (annexConfig "debugfilter")
	, annexWebOptions = getwords (annexConfig "web-options")
	, annexYoutubeDlOptions = getwords (annexConfig "youtube-dl-options")
	, annexYoutubeDlCommand = getmaybe (annexConfig "youtube-dl-command")
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
	, annexFreezeContentCommand = getmaybe (annexConfig "freezecontent-command")
	, annexThawContentCommand = getmaybe (annexConfig "thawcontent-command")
	, annexSecureEraseCommand = getmaybe (annexConfig "secure-erase-command")
	, annexGenMetaData = getbool (annexConfig "genmetadata") False
	, annexListen = getmaybe (annexConfig "listen")
	, annexPort = getmayberead (annexConfig "port")
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
	, annexDbDir = (\d -> toRawFilePath d P.</> fromUUID hereuuid)
		<$> getmaybe (annexConfig "dbdir")
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
	, annexMaxExtensions = getmayberead (annexConfig "maxextensions")
	, annexJobs = fromMaybe NonConcurrent $ 
		parseConcurrency =<< getmaybe (annexConfig "jobs")
	, annexCacheCreds = getbool (annexConfig "cachecreds") True
	, annexAutoUpgradeRepository = getbool (annexConfig "autoupgraderepository") True
	, annexCommitMode = if getbool (annexConfig "allowsign") False
		then ManualCommit
		else AutomaticCommit
	, annexSkipUnknown = getbool (annexConfig "skipunknown") False
	, annexAdjustedBranchRefresh = fromMaybe
		-- parse as bool if it's not a number
		(if getbool "adjustedbranchrefresh" False then 1 else 0)
		(getmayberead (annexConfig "adjustedbranchrefresh"))
	, annexSupportUnlocked = getbool (annexConfig "supportunlocked") True
	, coreSymlinks = getbool "core.symlinks" True
	, coreSharedRepository = getSharedRepository r
	, coreQuotePath = QuotePath (getbool "core.quotepath" True)
	, receiveDenyCurrentBranch = getDenyCurrentBranch r
	, gcryptId = getmaybe "core.gcrypt-id"
	, gpgCmd = mkGpgCmd (getmaybe "gpg.program")
	, mergeDirectoryRenames = getmaybe "directoryrenames"
	, annexPrivateRepos = S.fromList $ concat
		[ if getbool (annexConfig "private") False
			then [hereuuid]
			else []
		, let get (k, v)
			| Git.Config.isTrueFalse' v /= Just True = Nothing
			| isRemoteKey (remoteAnnexConfigEnd "private") k = do
				remotename <- remoteKeyToRemoteName k
				toUUID <$> Git.Config.getMaybe
					(remoteAnnexConfig remotename "uuid") r
			| otherwise = Nothing
		  in mapMaybe get (M.toList (Git.config r))
		]
	, annexAdviceNoSshCaching = getbool (annexConfig "advicenosshcaching") True
	, annexViewUnsetDirectory = ViewUnset $ fromMaybe "_" $
		getmaybe (annexConfig "viewunsetdirectory")
	, annexClusters = 
		M.mapMaybe (mkClusterUUID . toUUID) $
			M.mapKeys removeclusterprefix $
				M.filterWithKey isclusternamekey (config r)
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
	
	configurablemaybe Nothing = DefaultConfig Nothing
	configurablemaybe (Just v) = case configsource of
		FromGitConfig -> HasGitConfig (Just v)
		FromGlobalConfig -> HasGlobalConfig (Just v)

	onemegabyte = 1000000
	
	hereuuid = maybe NoUUID toUUID $ getmaybe (annexConfig "uuid")

	clusterprefix = annexConfigPrefix <> "cluster."
	isclusternamekey k _ = clusterprefix `B.isPrefixOf` (fromConfigKey' k)
		&& isLegalName (removeclusterprefix k)
	removeclusterprefix k = drop (B.length clusterprefix) (fromConfigKey k)

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
	[ annexConfig "largefiles"
	, annexConfig "dotfiles"
	, annexConfig "addunlocked"
	, annexConfig "autocommit"
	, annexConfig "resolvemerge"
	, annexConfig "synccontent"
	, annexConfig "synconlyannex"
	, annexConfig "securehashesonly"
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
	, remoteAnnexSpeculatePresent :: Bool
	, remoteAnnexBare :: Maybe Bool
	, remoteAnnexRetry :: Maybe Integer
	, remoteAnnexForwardRetry :: Maybe Integer
	, remoteAnnexRetryDelay :: Maybe Seconds
	, remoteAnnexStallDetection :: Maybe StallDetection
	, remoteAnnexStallDetectionUpload :: Maybe StallDetection
	, remoteAnnexStallDetectionDownload :: Maybe StallDetection
	, remoteAnnexBwLimit :: Maybe BwRate
	, remoteAnnexBwLimitUpload :: Maybe BwRate
	, remoteAnnexBwLimitDownload :: Maybe BwRate
	, remoteAnnexAllowUnverifiedDownloads :: Bool
	, remoteAnnexUUID :: Maybe UUID
	, remoteAnnexConfigUUID :: Maybe UUID
	, remoteAnnexMaxGitBundles :: Int
	, remoteAnnexAllowEncryptedGitRepo :: Bool
	, remoteAnnexProxy :: Bool
	, remoteAnnexProxiedBy :: Maybe UUID
	, remoteAnnexClusterNode :: Maybe [RemoteName]
	, remoteAnnexClusterGateway :: [ClusterUUID]
	, remoteUrl :: Maybe String
	, remoteAnnexP2PHttpUrl :: Maybe P2PHttpUrl

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
	, remoteAnnexSharedSOPCommand :: Maybe SOPCmd
	, remoteAnnexSharedSOPProfile :: Maybe SOPProfile
	, remoteAnnexRsyncUrl :: Maybe String
	, remoteAnnexBupRepo :: Maybe String
	, remoteAnnexBorgRepo :: Maybe String
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
		(notempty $ getmaybe CostCommandField)
		(getmayberead CostField)
	annexignore <- mkDynamicConfig unsuccessfullCommandRunner
		(notempty $ getmaybe IgnoreCommandField)
		(getbool IgnoreField False)
	annexsync <- mkDynamicConfig successfullCommandRunner
		(notempty $ getmaybe SyncCommandField)
		(getbool SyncField True)
	return $ RemoteGitConfig
		{ remoteAnnexCost = annexcost
		, remoteAnnexIgnore = annexignore
		, remoteAnnexSync = annexsync
		, remoteAnnexPull = getbool PullField True
		, remoteAnnexPush = getbool PushField True
		, remoteAnnexReadOnly = getbool ReadOnlyField False
		, remoteAnnexCheckUUID = getbool CheckUUIDField True
		, remoteAnnexVerify = getbool VerifyField True
		, remoteAnnexTrackingBranch = Git.Ref . encodeBS <$>
			( notempty (getmaybe TrackingBranchField)
			<|> notempty (getmaybe ExportTrackingField) -- old name
			)
		, remoteAnnexTrustLevel = notempty $ getmaybe TrustLevelField
		, remoteAnnexStartCommand = notempty $ getmaybe StartCommandField
		, remoteAnnexStopCommand = notempty $ getmaybe StopCommandField
		, remoteAnnexSpeculatePresent = 
			getbool SpeculatePresentField False
		, remoteAnnexBare = getmaybebool BareField
		, remoteAnnexRetry = getmayberead RetryField
		, remoteAnnexForwardRetry = getmayberead ForwardRetryField
		, remoteAnnexRetryDelay = Seconds
			<$> getmayberead RetryDelayField
		, remoteAnnexStallDetection =
			readStallDetection =<< getmaybe StallDetectionField
		, remoteAnnexStallDetectionUpload =
			readStallDetection =<< getmaybe StallDetectionUploadField
		, remoteAnnexStallDetectionDownload =
			readStallDetection =<< getmaybe StallDetectionDownloadField
		, remoteAnnexBwLimit =
			readBwRatePerSecond =<< getmaybe BWLimitField
		, remoteAnnexBwLimitUpload =
			readBwRatePerSecond =<< getmaybe BWLimitUploadField
		, remoteAnnexBwLimitDownload =
			readBwRatePerSecond =<< getmaybe BWLimitDownloadField
		, remoteAnnexAllowUnverifiedDownloads = (== Just "ACKTHPPT") $
			getmaybe SecurityAllowUnverifiedDownloadsField
		, remoteAnnexUUID = toUUID <$> getmaybe UUIDField
		, remoteAnnexConfigUUID = toUUID <$> getmaybe ConfigUUIDField
		, remoteAnnexMaxGitBundles =
			fromMaybe 100 (getmayberead MaxGitBundlesField)
		, remoteAnnexAllowEncryptedGitRepo = 
			getbool AllowEncryptedGitRepoField False
		, remoteAnnexProxy = getbool ProxyField False
		, remoteAnnexProxiedBy = toUUID <$> getmaybe ProxiedByField
		, remoteAnnexClusterNode = 
			(filter isLegalName . words)
				<$> getmaybe ClusterNodeField
		, remoteAnnexClusterGateway = fromMaybe [] $
			(mapMaybe (mkClusterUUID . toUUID) . words)
				<$> getmaybe ClusterGatewayField
		, remoteUrl = 
			case Git.Config.getMaybe (remoteConfig remotename (remoteGitConfigKey UrlField)) r of
				Just (ConfigValue b)
					| B.null b -> Nothing
					| otherwise -> Just (decodeBS b)
				_ -> Nothing
		, remoteAnnexP2PHttpUrl =
			case Git.Config.getMaybe (remoteConfig remotename (remoteGitConfigKey AnnexUrlField)) r of
				Just (ConfigValue b) ->
					parseP2PHttpUrl (decodeBS b)
				_ -> Nothing
		, remoteAnnexShell = getmaybe ShellField
		, remoteAnnexSshOptions = getoptions SshOptionsField
		, remoteAnnexRsyncOptions = getoptions RsyncOptionsField
		, remoteAnnexRsyncDownloadOptions = getoptions RsyncDownloadOptionsField
		, remoteAnnexRsyncUploadOptions = getoptions RsyncUploadOptionsField
		, remoteAnnexRsyncTransport = getoptions RsyncTransportField
		, remoteAnnexGnupgOptions = getoptions GnupgOptionsField
		, remoteAnnexGnupgDecryptOptions = getoptions GnupgDecryptOptionsField
		, remoteAnnexSharedSOPCommand = SOPCmd <$>
			notempty (getmaybe SharedSOPCommandField)
		, remoteAnnexSharedSOPProfile = SOPProfile <$>
			notempty (getmaybe SharedSOPProfileField)
		, remoteAnnexRsyncUrl = notempty $ getmaybe RsyncUrlField
		, remoteAnnexBupRepo = getmaybe BupRepoField
		, remoteAnnexBorgRepo = getmaybe BorgRepoField
		, remoteAnnexTahoe = getmaybe TahoeField
		, remoteAnnexBupSplitOptions = getoptions BupSplitOptionsField
		, remoteAnnexDirectory = notempty $ getmaybe DirectoryField
		, remoteAnnexAndroidDirectory = notempty $ getmaybe AndroidDirectoryField
		, remoteAnnexAndroidSerial = notempty $ getmaybe AndroidSerialField
		, remoteAnnexGCrypt = notempty $ getmaybe GCryptField
		, remoteAnnexGitLFS = getbool GitLFSField False
		, remoteAnnexDdarRepo = getmaybe DdarRepoField
		, remoteAnnexHookType = notempty $ getmaybe HookTypeField
		, remoteAnnexExternalType = notempty $ getmaybe ExternalTypeField
		}
  where
	getbool k d = fromMaybe d $ getmaybebool k
	getmaybebool k = Git.Config.isTrueFalse' =<< getmaybe' k
	getmayberead k = readish =<< getmaybe k
	getmaybe = fmap fromConfigValue . getmaybe'
	getmaybe' :: RemoteGitConfigField -> Maybe ConfigValue
	getmaybe' f =
		let k = remoteGitConfigKey f
		in Git.Config.getMaybe (remoteAnnexConfig remotename k) r
			<|> Git.Config.getMaybe (annexConfig k) r
	getoptions k = fromMaybe [] $ words <$> getmaybe k

data RemoteGitConfigField
	= CostField
	| CostCommandField
	| IgnoreField
	| IgnoreCommandField
	| SyncField
	| SyncCommandField
	| PullField
	| PushField
	| ReadOnlyField
	| CheckUUIDField
	| VerifyField
	| TrackingBranchField
	| ExportTrackingField
	| TrustLevelField
	| StartCommandField
	| StopCommandField
	| SpeculatePresentField
	| BareField
	| RetryField
	| ForwardRetryField
	| RetryDelayField
	| StallDetectionField
	| StallDetectionUploadField
	| StallDetectionDownloadField
	| BWLimitField
	| BWLimitUploadField
	| BWLimitDownloadField
	| UUIDField
	| ConfigUUIDField
	| SecurityAllowUnverifiedDownloadsField
	| MaxGitBundlesField
	| AllowEncryptedGitRepoField
	| ProxyField
	| ProxiedByField
	| ClusterNodeField
	| ClusterGatewayField
	| UrlField
	| AnnexUrlField
	| ShellField
	| SshOptionsField
	| RsyncOptionsField
	| RsyncDownloadOptionsField
	| RsyncUploadOptionsField
	| RsyncTransportField
	| GnupgOptionsField
	| GnupgDecryptOptionsField
	| SharedSOPCommandField
	| SharedSOPProfileField
	| RsyncUrlField
	| BupRepoField
	| BorgRepoField
	| TahoeField
	| BupSplitOptionsField
	| DirectoryField
	| AndroidDirectoryField
	| AndroidSerialField
	| GCryptField
	| GitLFSField
	| DdarRepoField
	| HookTypeField
	| ExternalTypeField
	deriving (Enum, Bounded)

remoteGitConfigField :: RemoteGitConfigField -> (UnqualifiedConfigKey, ProxyInherited)
remoteGitConfigField = \case
	-- Hard to know the true cost of accessing eg a slow special
	-- remote via the proxy. The cost of the proxy is the best guess
	-- so do inherit it.
	CostField -> inherited "cost"
	CostCommandField -> inherited "cost-command"
	IgnoreField -> inherited "ignore"
	IgnoreCommandField -> inherited "ignore-command"
	SyncField -> inherited "sync"
	SyncCommandField -> inherited "sync-command"
	PullField -> inherited "pull"
	PushField -> inherited "push"
	ReadOnlyField -> inherited "readonly"
	CheckUUIDField -> uninherited "checkuuid"
	VerifyField -> inherited "verify"
	TrackingBranchField -> uninherited "tracking-branch"
	ExportTrackingField -> uninherited "export-tracking"
	TrustLevelField -> uninherited "trustlevel"
	StartCommandField -> uninherited "start-command"
	StopCommandField -> uninherited "stop-command"
	SpeculatePresentField -> inherited "speculate-present"
	BareField -> inherited "bare"
	RetryField -> inherited "retry"
	ForwardRetryField -> inherited "forward-retry"
	RetryDelayField -> inherited "retrydelay"
	StallDetectionField -> inherited "stalldetection"
	StallDetectionUploadField -> inherited "stalldetection-upload"
	StallDetectionDownloadField -> inherited "stalldetection-download"
	BWLimitField -> inherited "bwlimit"
	BWLimitUploadField -> inherited "bwlimit-upload"
	BWLimitDownloadField -> inherited "bwlimit-upload"
	UUIDField -> uninherited "uuid"
	ConfigUUIDField -> uninherited "config-uuid"
	SecurityAllowUnverifiedDownloadsField -> inherited "security-allow-unverified-downloads"
	MaxGitBundlesField -> inherited "max-git-bundles"
	AllowEncryptedGitRepoField -> inherited "allow-encrypted-gitrepo"
	-- Allow proxy chains.
	ProxyField -> inherited "proxy"
	ProxiedByField -> uninherited "proxied-by"
	ClusterNodeField -> uninherited "cluster-node"
	ClusterGatewayField -> uninherited "cluster-gateway"
	UrlField -> uninherited "url"
	AnnexUrlField -> uninherited "annexurl"
	ShellField -> inherited "shell"
	SshOptionsField -> inherited "ssh-options"
	RsyncOptionsField -> inherited "rsync-options"
	RsyncDownloadOptionsField -> inherited "rsync-download-options"
	RsyncUploadOptionsField -> inherited "rsync-upload-options"
	RsyncTransportField -> inherited "rsync-transport"
	GnupgOptionsField -> inherited "gnupg-options"
	GnupgDecryptOptionsField -> inherited "gnupg-decrypt-options"
	SharedSOPCommandField -> inherited "shared-sop-command"
	SharedSOPProfileField -> inherited "shared-sop-profile"
	RsyncUrlField -> uninherited "rsyncurl"
	BupRepoField -> uninherited "buprepo"
	BorgRepoField -> uninherited "borgrepo"
	TahoeField -> uninherited "tahoe"
	BupSplitOptionsField -> uninherited "bup-split-options"
	DirectoryField -> uninherited "directory"
	AndroidDirectoryField -> uninherited "androiddirectory"
	AndroidSerialField -> uninherited "androidserial"
	GCryptField -> uninherited "gcrypt"
	GitLFSField -> uninherited "git-lfs"
	DdarRepoField -> uninherited "ddarrepo"
	HookTypeField -> uninherited "hooktype"
	ExternalTypeField -> uninherited "externaltype"
  where
	inherited f = (f, ProxyInherited True)
	uninherited f = (f, ProxyInherited False)

newtype ProxyInherited = ProxyInherited Bool

-- All remote config fields that are inherited from a proxy.
proxyInheritedFields :: [UnqualifiedConfigKey]
proxyInheritedFields = 
	map fst $
		filter (\(_, ProxyInherited p) -> p) $
			map remoteGitConfigField [minBound..maxBound]

remoteGitConfigKey :: RemoteGitConfigField -> UnqualifiedConfigKey
remoteGitConfigKey = fst . remoteGitConfigField

notempty :: Maybe String -> Maybe String	
notempty Nothing = Nothing
notempty (Just "") = Nothing
notempty (Just s) = Just s

dummyRemoteGitConfig :: IO RemoteGitConfig
dummyRemoteGitConfig = atomically $ 
	extractRemoteGitConfig Git.Construct.fromUnknown "dummy"

type UnqualifiedConfigKey = B.ByteString

annexConfigPrefix :: B.ByteString
annexConfigPrefix = "annex."

{- A global annex setting in git config. -}
annexConfig :: UnqualifiedConfigKey -> ConfigKey
annexConfig key = ConfigKey (annexConfigPrefix <> key)

class RemoteNameable r where
	getRemoteName :: r -> RemoteName

instance RemoteNameable Git.Repo where
	getRemoteName r = fromMaybe "" (Git.remoteName r)

instance RemoteNameable RemoteName where
	 getRemoteName = id

{- A per-remote annex setting in git config. -}
remoteAnnexConfig :: RemoteNameable r => r -> UnqualifiedConfigKey -> ConfigKey
remoteAnnexConfig r = remoteConfig r . remoteAnnexConfigEnd

remoteAnnexConfigEnd :: UnqualifiedConfigKey -> UnqualifiedConfigKey
remoteAnnexConfigEnd key = "annex-" <> key

{- A per-remote setting in git config. -}
remoteConfig :: RemoteNameable r => r -> UnqualifiedConfigKey -> ConfigKey
remoteConfig r key = ConfigKey $
	"remote." <> encodeBS (getRemoteName r) <> "." <> key

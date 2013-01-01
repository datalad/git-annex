{- git-annex configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.GitConfig ( 
	GitConfig(..),
	extractGitConfig,
	RemoteGitConfig(..),
	extractRemoteGitConfig,
) where

import Common
import qualified Git
import qualified Git.Config
import Utility.DataUnits

{- Main git-annex settings. Each setting corresponds to a git-config key
 - such as annex.foo -}
data GitConfig = GitConfig
	{ annexVersion :: Maybe String
	, annexNumCopies :: Int
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
	}

extractGitConfig :: Git.Repo -> GitConfig
extractGitConfig r = GitConfig
	{ annexVersion = notempty $ getmaybe "version"
	, annexNumCopies = get "numcopies" 1
	, annexDiskReserve = fromMaybe onemegabyte $
		readSize dataUnits =<< getmaybe "diskreserve"
	, annexDirect = getbool "direct" False
	, annexBackends = fromMaybe [] $ words <$> getmaybe "backends"
	, annexQueueSize = getmayberead "queuesize"
	, annexBloomCapacity = getmayberead "bloomcapacity"
	, annexBloomAccuracy = getmayberead "bloomaccuracy"
	, annexSshCaching = getmaybebool "sshcaching"
	, annexAlwaysCommit = getbool "alwayscommit" True
	, annexDelayAdd = getmayberead "delayadd"
	, annexHttpHeaders = getlist "http-headers"
	, annexHttpHeadersCommand = getmaybe "http-headers-command"
	}
  where
	get k def = fromMaybe def $ getmayberead k
	getbool k def = fromMaybe def $ getmaybebool k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	getmayberead k = readish =<< getmaybe k
	getmaybe k = Git.Config.getMaybe (key k) r
	getlist k = Git.Config.getList (key k) r

	key k = "annex." ++ k
			
	onemegabyte = 1000000

{- Per-remote git-annex settings. Each setting corresponds to a git-config
 - key such as <remote>.annex-foo, or if that is not set, a default from
 - annex.foo -}
data RemoteGitConfig = RemoteGitConfig
	{ remoteAnnexCost :: Maybe Int
	, remoteAnnexCostCommand :: Maybe String
	, remoteAnnexIgnore :: Bool
	, remoteAnnexSync :: Bool
	, remoteAnnexTrustLevel :: Maybe String
	, remoteAnnexStartCommand :: Maybe String
	, remoteAnnexStopCommand :: Maybe String

	-- these settings are specific to particular types of remotes
	, remoteAnnexSshOptions :: [String]
	, remoteAnnexRsyncOptions :: [String]
	, remoteAnnexRsyncUrl :: Maybe String
	, remoteAnnexBupRepo :: Maybe String
	, remoteAnnexBupSplitOptions :: [String]
	, remoteAnnexDirectory :: Maybe FilePath
	, remoteAnnexHookType :: Maybe String
	}

extractRemoteGitConfig :: Git.Repo -> String -> RemoteGitConfig
extractRemoteGitConfig r remotename = RemoteGitConfig
	{ remoteAnnexCost = getmayberead "cost"
	, remoteAnnexCostCommand = notempty $ getmaybe "cost-command"
	, remoteAnnexIgnore = getbool "ignore" False
	, remoteAnnexSync = getbool "sync" True
	, remoteAnnexTrustLevel = notempty $ getmaybe "trustlevel"
	, remoteAnnexStartCommand = notempty $ getmaybe "start-command"
	, remoteAnnexStopCommand = notempty $ getmaybe "stop-command"

	, remoteAnnexSshOptions = getoptions "ssh-options"
	, remoteAnnexRsyncOptions = getoptions "rsync-options"
	, remoteAnnexRsyncUrl = notempty $ getmaybe "rsyncurl"
	, remoteAnnexBupRepo = getmaybe "buprepo"
	, remoteAnnexBupSplitOptions = getoptions "bup-split-options"
	, remoteAnnexDirectory = notempty $ getmaybe "directory"
	, remoteAnnexHookType = notempty $ getmaybe "hooktype"
	}
  where
	getbool k def = fromMaybe def $ getmaybebool k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	getmayberead k = readish =<< getmaybe k
	getmaybe k = maybe (Git.Config.getMaybe (key k) r) Just $
		Git.Config.getMaybe (remotekey k) r
	getoptions k = fromMaybe [] $ words <$> getmaybe k

	key k = "annex." ++ k
	remotekey k = "remote." ++ remotename ++ ".annex-" ++ k

notempty :: Maybe String -> Maybe String	
notempty Nothing = Nothing
notempty (Just "") = Nothing
notempty (Just s) = Just s


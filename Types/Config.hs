{- git-annex configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Config ( 
	Config(..),
	extractConfig,
) where

import Common
import qualified Git
import qualified Git.Config
import Utility.DataUnits

{- Main git-annex settings. Each setting corresponds to a git-config key
 - such as annex.foo -}
data Config = Config
	{ annexNumCopies :: Int
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

extractConfig :: Git.Repo -> Config
extractConfig r = Config
	{ annexNumCopies = get "numcopies" 1
	, annexDiskReserve = fromMaybe onemegabyte $
		readSize dataUnits =<< getmaybe "diskreserve"
	, annexDirect = getbool "direct" False
	, annexBackends = fromMaybe [] $
		words <$> getmaybe "backends"
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
 - key such as annex.<remote>.foo -}

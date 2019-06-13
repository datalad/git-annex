{- Rsync urls.
 -
 - Copyright 2014-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Rsync.RsyncUrl where

import Types
import Annex.Locations
import Utility.Rsync
import Utility.SafeCommand

import Data.Default
import System.FilePath.Posix
#ifdef mingw32_HOST_OS
import Utility.Split
#endif
import Annex.DirHashes

type RsyncUrl = String

data RsyncOpts = RsyncOpts
	{ rsyncUrl :: RsyncUrl
	, rsyncOptions :: Annex [CommandParam]
	, rsyncUploadOptions :: Annex [CommandParam]
	, rsyncDownloadOptions :: Annex [CommandParam]
	, rsyncShellEscape :: Bool
}

rsyncEscape :: RsyncOpts -> RsyncUrl -> RsyncUrl
rsyncEscape o u
	| rsyncShellEscape o && rsyncUrlIsShell (rsyncUrl o) = shellEscape u
	| otherwise = u

mkRsyncUrl :: RsyncOpts -> FilePath -> RsyncUrl
mkRsyncUrl o f = rsyncUrl o </> rsyncEscape o f

rsyncUrls :: RsyncOpts -> Key -> [RsyncUrl]
rsyncUrls o k = map use dirHashes
  where
	use h = rsyncUrl o </> hash h </> rsyncEscape o (f </> f)
	f = keyFile k
#ifndef mingw32_HOST_OS
	hash h = h def k
#else
	hash h = replace "\\" "/" (h def k)
#endif

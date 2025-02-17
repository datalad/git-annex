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
import Utility.ShellEscape
import Utility.OsPath
import Annex.DirHashes
#ifdef mingw32_HOST_OS
import Utility.Split
#endif

import Data.Default
import qualified System.FilePath.Posix as Posix
import qualified Data.List.NonEmpty as NE

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
mkRsyncUrl o f = rsyncUrl o Posix.</> rsyncEscape o f

rsyncUrls :: RsyncOpts -> Key -> [RsyncUrl]
rsyncUrls o k = map use (NE.toList dirHashes)
  where
	use h = rsyncUrl o Posix.</> hash h Posix.</> rsyncEscape o (f Posix.</> f)
	f = fromOsPath (keyFile k)
#ifndef mingw32_HOST_OS
	hash h = fromOsPath $ h def k
#else
	hash h = replace "\\" "/" $ fromOsPath $ h def k
#endif

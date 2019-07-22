{- Bundled programs
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Build.BundledPrograms where

import Data.Maybe

import BuildInfo

{- Programs that git-annex uses, to include in the bundle.
 -
 - These may be just the command name, or the full path to it. -}
bundledPrograms :: [FilePath]
bundledPrograms = preferredBundledPrograms ++ extraBundledPrograms

{- Programs that are only included in the bundle in case the system
 - doesn't have them. These come after the system PATH.
 -}
extraBundledPrograms :: [FilePath]
extraBundledPrograms = catMaybes
	[ Nothing
#ifndef darwin_HOST_OS
#ifndef mingw32_HOST_OS
	-- OS X has ssh installed by default.
	-- On Windows, git provides ssh.
	-- Linux probably has ssh installed system wide,
	-- and if so the user probably wants to use that one.
	, Just "ssh"
	, Just "ssh-keygen"
#endif
#endif
	]

{- Programs that should be preferred for use from the bundle, over
 - any that might be installed on the system otherwise. These come before
 - the system PATH.
 -
 - For example, git-annex is built for a specific version of git.
 -}
preferredBundledPrograms :: [FilePath]
preferredBundledPrograms = catMaybes
	[ Nothing
#ifndef mingw32_HOST_OS
	-- git is not included in the windows bundle; git for windows is used
	, Just "git"
	-- Not strictly needed in PATH by git-annex, but called
	-- by git when it sshes to a remote.
	, Just "git-upload-pack"
	, Just "git-receive-pack"
	, Just "git-shell"
	-- using xargs on windows led to problems, so it's not used there
	, Just "xargs"
	-- rsync is not a core dependency, but good to have.
	-- On windows, bundling rsync required the build be used with a
	-- particular version of git for windows, so not included there.
	, Just "rsync"
	, Just "sh"
	-- used by git-annex when available
	, Just "uname"
#endif
	, BuildInfo.lsof
	, BuildInfo.gcrypt
#ifndef mingw32_HOST_OS
	-- These utilities are included in git for Windows
	, ifset BuildInfo.curl "curl"
	, Just "cp"
#endif
#ifdef linux_HOST_OS
	-- used to unpack the tarball when upgrading
	, Just "gunzip"
	, Just "tar"
	-- used by runshell to generate locales
	, Just "localedef"
#endif
	-- nice, ionice, and nocache are not included in the bundle;
	-- we rely on the system's own version, which may better match
	-- its kernel, and avoid using them if not available.
	]
  where
	ifset True s = Just s
	ifset False _ = Nothing

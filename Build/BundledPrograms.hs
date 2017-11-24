{- Bundled programs
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Build.BundledPrograms where

import Data.Maybe

import Build.SysConfig as SysConfig

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
	-- The system gpg is probably better, because it may better
	-- integrate with the system gpg-agent, etc.
	-- On Windows, gpg is bundled with git for windows.
#ifndef mingw32_HOST_OS
	[ SysConfig.gpg
#else
	[
#endif
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
#endif
#ifndef mingw32_HOST_OS
	-- using xargs on windows led to problems, so it's not used there
	, Just "xargs"
#endif
	, Just "rsync"
#ifndef mingw32_HOST_OS
	, Just "sh"
#endif
#ifndef mingw32_HOST_OS
#ifndef darwin_HOST_OS
	-- wget on OSX has been problematic, looking for certs in the wrong
	-- places. Don't ship it, use curl or the OSX's own wget if it has
	-- one.
	, ifset SysConfig.wget "wget"
#endif
#endif
	, SysConfig.lsof
	, SysConfig.gcrypt
#ifndef mingw32_HOST_OS
	-- All these utilities are included in git for Windows
	, ifset SysConfig.curl "curl"
	, SysConfig.sha1
	, SysConfig.sha256
	, SysConfig.sha512
	, SysConfig.sha224
	, SysConfig.sha384
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

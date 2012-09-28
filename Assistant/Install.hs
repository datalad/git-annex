{- Assistant installation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Install where

import Assistant.Common
import Assistant.Install.AutoStart
import Assistant.Ssh
import Locations.UserConfig
import Utility.FileMode

#ifdef darwin_HOST_OS
import Utility.OSX
#else
import Utility.FreeDesktop
#endif

import System.Posix.Env

standaloneAppBase :: IO (Maybe FilePath)
standaloneAppBase = getEnv "GIT_ANNEX_APP_BASE"

{- The standalone app does not have an installation process.
 - So when it's run, it needs to set up autostarting of the assistant
 - daemon, as well as writing the programFile, and putting a
 - git-annex-shell wrapper into ~/.ssh
 -
 - Note that this is done every time it's started, so if the user moves
 - it around, the paths this sets up won't break.
 -}
ensureInstalled :: IO ()
ensureInstalled = go =<< standaloneAppBase
	where
		go Nothing = noop
		go (Just base) = do
			let program = base ++ "runshell git-annex"
			programfile <- programFile
			createDirectoryIfMissing True (parentDir programfile)
			writeFile programfile program

#ifdef darwin_HOST_OS
			autostartfile <- userAutoStart osxAutoStartLabel
#else
			autostartfile <- autoStartPath "git-annex"
				<$> userConfigDir
#endif
			installAutoStart program autostartfile

			{- This shim is only updated if it doesn't
			 - already exist with the right content. This
			 - ensures that there's no race where it would have
			 - worked, but is unavailable due to being updated. -}
			sshdir <- sshDir
			let shim = sshdir </> "git-annex-shell"
			let content = unlines
				[ "#!/bin/sh"
				, "set -e"
				, "exec", base </> "runshell" ++ 
				  " git-annex-shell -c \"$SSH_ORIGINAL_COMMAND\""
				]
			curr <- catchDefaultIO "" $ readFileStrict shim
			when (curr /= content) $ do
				createDirectoryIfMissing True (parentDir shim)
				writeFile shim content
				modifyFileMode shim $ addModes [ownerExecuteMode]

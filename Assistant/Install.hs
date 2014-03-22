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
import Config.Files
import Utility.FileMode
import Utility.Shell
import Utility.Tmp
import Utility.Env
import Utility.SshConfig

#ifdef darwin_HOST_OS
import Utility.OSX
#else
import Utility.FreeDesktop
import Assistant.Install.Menu
#endif

standaloneAppBase :: IO (Maybe FilePath)
standaloneAppBase = getEnv "GIT_ANNEX_APP_BASE"

{- The standalone app does not have an installation process.
 - So when it's run, it needs to set up autostarting of the assistant
 - daemon, as well as writing the programFile, and putting a
 - git-annex-shell wrapper into ~/.ssh
 -
 - Note that this is done every time it's started, so if the user moves
 - it around, the paths this sets up won't break.
 -
 - Nautilus hook script installation is done even for packaged apps,
 - since it has to go into the user's home directory.
 -}
ensureInstalled :: IO ()
ensureInstalled = go =<< standaloneAppBase
  where
	go Nothing = installNautilus "git-annex"
	go (Just base) = do
		let program = base </> "git-annex"
		programfile <- programFile
		createDirectoryIfMissing True (parentDir programfile)
		writeFile programfile program

#ifdef darwin_HOST_OS
		autostartfile <- userAutoStart osxAutoStartLabel
#else
		menufile <- desktopMenuFilePath "git-annex" <$> userDataDir
		icondir <- iconDir <$> userDataDir
		installMenu program menufile base icondir
		autostartfile <- autoStartPath "git-annex" <$> userConfigDir
#endif
		installAutoStart program autostartfile

		{- This shim is only updated if it doesn't
		 - already exist with the right content. -}
		sshdir <- sshDir
		let shim = sshdir </> "git-annex-shell"
		let runshell var = "exec " ++ base </> "runshell" ++
			" git-annex-shell -c \"" ++ var ++ "\""
		let content = unlines
			[ shebang_local
			, "set -e"
			, "if [ \"x$SSH_ORIGINAL_COMMAND\" != \"x\" ]; then"
			,   runshell "$SSH_ORIGINAL_COMMAND"
			, "else"
			,   runshell "$@"
			, "fi"
			]

		curr <- catchDefaultIO "" $ readFileStrict shim
		when (curr /= content) $ do
			createDirectoryIfMissing True (parentDir shim)
			viaTmp writeFile shim content
			modifyFileMode shim $ addModes [ownerExecuteMode]

		installNautilus program

installNautilus :: FilePath -> IO ()
#ifdef linux_HOST_OS
installNautilus program = do
	scriptdir <- (\d -> d </> "nautilus" </> "scripts") <$> userDataDir
	genscript scriptdir "get"
	genscript scriptdir "drop"
  where
	genscript scriptdir action =
		installscript (scriptdir </> scriptname action) $ unlines
			[ "#!/bin/sh"
			, autoaddedcomment
			, program ++ " " ++ action ++ " --notify-start --notify-finish \"$@\""
			]
	scriptname action = "git-annex " ++ action
	installscript f c = whenM (safetoinstallscript f) $ do
		writeFile f c
		modifyFileMode f $ addModes [ownerExecuteMode]
	safetoinstallscript f = catchDefaultIO True $
		elem autoaddedcomment . lines <$> readFileStrict f
	autoaddedcomment = "# Automatically added by git-annex, do not edit. (To disable, chmod 600 this file.)"
#else
installNautilus _ = noop
#endif

{- Returns a cleaned up environment that lacks settings used to make the
 - standalone builds use their bundled libraries and programs.
 - Useful when calling programs not included in the standalone builds.
 -
 - For a non-standalone build, returns Nothing.
 -}
cleanEnvironment :: IO (Maybe [(String, String)])
cleanEnvironment = clean <$> getEnvironment
  where
	clean env
		| null vars = Nothing
		| otherwise = Just $ catMaybes $ map (restoreorig env) env
		| otherwise = Nothing
	  where
		vars = words $ fromMaybe "" $
			lookup "GIT_ANNEX_STANDLONE_ENV" env
		restoreorig oldenv p@(k, _v)
			| k `elem` vars = case lookup ("ORIG_" ++ k) oldenv of
				(Just v')
					| not (null v') -> Just (k, v')
				_ -> Nothing
			| otherwise = Just p

{- Assistant installation
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
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
#ifdef linux_HOST_OS
import Utility.UserInfo
#endif
import Assistant.Install.Menu
#endif

standaloneAppBase :: IO (Maybe FilePath)
standaloneAppBase = getEnv "GIT_ANNEX_APP_BASE"

{- The standalone app does not have an installation process.
 - So when it's run, it needs to set up autostarting of the assistant
 - daemon, as well as writing the programFile, and putting the
 - git-annex-shell and git-annex-wrapper wrapper scripts into ~/.ssh
 -
 - Note that this is done every time it's started, so if the user moves
 - it around, the paths this sets up won't break.
 -
 - File manager hook script installation is done even for
 - packaged apps, since it has to go into the user's home directory.
 -}
ensureInstalled :: IO ()
ensureInstalled = ifM (isJust <$> getEnv "GIT_ANNEX_PACKAGE_INSTALL")
	( go Nothing
	, go =<< standaloneAppBase
	)
  where
	go Nothing = installFileManagerHooks "git-annex"
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

		sshdir <- sshDir
		let runshell var = "exec " ++ base </> "runshell " ++ var
		let rungitannexshell var = runshell $ "git-annex-shell -c \"" ++ var ++ "\""

		installWrapper (sshdir </> "git-annex-shell") $ unlines
			[ shebang_local
			, "set -e"
			, "if [ \"x$SSH_ORIGINAL_COMMAND\" != \"x\" ]; then"
			,   rungitannexshell "$SSH_ORIGINAL_COMMAND"
			, "else"
			,   rungitannexshell "$@"
			, "fi"
			]
		installWrapper (sshdir </> "git-annex-wrapper") $ unlines
			[ shebang_local
			, "set -e"
			, runshell "\"$@\""
			]

		installFileManagerHooks program

installWrapper :: FilePath -> String -> IO ()
installWrapper file content = do
	curr <- catchDefaultIO "" $ readFileStrict file
	when (curr /= content) $ do
		createDirectoryIfMissing True (parentDir file)
		viaTmp writeFile file content
		modifyFileMode file $ addModes [ownerExecuteMode]

installFileManagerHooks :: FilePath -> IO ()
#ifdef linux_HOST_OS
installFileManagerHooks program = do
	let actions = ["get", "drop", "undo"]

	-- Gnome
	nautilusScriptdir <- (\d -> d </> "nautilus" </> "scripts") <$> userDataDir
	createDirectoryIfMissing True nautilusScriptdir
	forM_ actions $
		genNautilusScript nautilusScriptdir

	-- KDE
	home <- myHomeDir
	let kdeServiceMenusdir = home </> ".kde" </> "share" </> "kde4" </> "services" </> "ServiceMenus"
	createDirectoryIfMissing True kdeServiceMenusdir
	writeFile (kdeServiceMenusdir </> "git-annex.desktop")
		(kdeDesktopFile actions)
  where
	genNautilusScript scriptdir action =
		installscript (scriptdir </> scriptname action) $ unlines
			[ shebang_local
			, autoaddedcomment
			, "exec " ++ program ++ " " ++ action ++ " --notify-start --notify-finish -- \"$@\""
			]
	scriptname action = "git-annex " ++ action
	installscript f c = whenM (safetoinstallscript f) $ do
		writeFile f c
		modifyFileMode f $ addModes [ownerExecuteMode]
	safetoinstallscript f = catchDefaultIO True $
		elem autoaddedcomment . lines <$> readFileStrict f
	autoaddedcomment = "# " ++ autoaddedmsg ++ " (To disable, chmod 600 this file.)"
	autoaddedmsg = "Automatically added by git-annex, do not edit."

	kdeDesktopFile actions = unlines $ concat $
		kdeDesktopHeader actions : map kdeDesktopAction actions
	kdeDesktopHeader actions =
		[ "# " ++ autoaddedmsg
		, "[Desktop Entry]"
		, "Type=Service"
		, "ServiceTypes=all/allfiles"
		, "MimeType=all/all;"
		, "Actions=" ++ intercalate ";" (map kdeDesktopSection actions)
		, "X-KDE-Priority=TopLevel"
		, "X-KDE-Submenu=Git-Annex"
		, "X-KDE-Icon=git-annex"
		, "X-KDE-ServiceTypes=KonqPopupMenu/Plugin"
		]
	kdeDesktopSection command = "GitAnnex" ++ command
	kdeDesktopAction command = 
		[ ""
		, "[Desktop Action " ++ kdeDesktopSection command ++ "]"
		, "Name=" ++ command
		, "Icon=git-annex"
		, unwords
			[ "Exec=sh -c 'cd \"$(dirname \"$1\")\" &&"
			, program
			, command
			, "--notify-start --notify-finish -- \"$1\"'"
			, "false" -- this becomes $0 in sh, so unused
			, "%f"
			]
		]
#else
installFileManagerHooks _ = noop
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
	clean environ
		| null vars = Nothing
		| otherwise = Just $ catMaybes $ map (restoreorig environ) environ
		| otherwise = Nothing
	  where
		vars = words $ fromMaybe "" $
			lookup "GIT_ANNEX_STANDLONE_ENV" environ
		restoreorig oldenviron p@(k, _v)
			| k `elem` vars = case lookup ("ORIG_" ++ k) oldenviron of
				(Just v')
					| not (null v') -> Just (k, v')
				_ -> Nothing
			| otherwise = Just p

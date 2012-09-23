{- Generating and installing a desktop menu entry file
 - and a desktop autostart file. (And OSX equivilants.)
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Build.InstallDesktopFile where

import Utility.Exception
import Utility.FreeDesktop
import Utility.Path
import Utility.Monad
import Locations.UserConfig

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.Posix.User
import System.Posix.Types
import System.Posix.Files
import System.FilePath

{- The command can be either just "git-annex", or the full path to use
 - to run it. -}
desktop :: FilePath -> DesktopEntry
desktop command = genDesktopEntry
	"Git Annex"
	"Track and sync the files in your Git Annex"
	False
	(command ++ " webapp")
	["Network", "FileTransfer"]

autostart :: FilePath -> DesktopEntry
autostart command = genDesktopEntry
	"Git Annex Assistant"
	"Autostart"
	False
	(command ++ " assistant --autostart")
	[]

isRoot :: IO Bool
isRoot = do
	uid <- fromIntegral <$> getRealUserID
	return $ uid == 0

inDestDir :: FilePath -> IO FilePath
inDestDir f = do
	destdir <- catchDefaultIO "" (getEnv "DESTDIR")
	return $ destdir </> f

writeFDODesktop :: FilePath -> IO ()
writeFDODesktop command = do
	datadir <- ifM isRoot ( return systemDataDir, userDataDir )
	writeDesktopMenuFile (desktop command) 
		=<< inDestDir (desktopMenuFilePath "git-annex" datadir)

	configdir <- ifM isRoot ( return systemConfigDir, userConfigDir )
	writeDesktopMenuFile (autostart command) 
		=<< inDestDir (autoStartPath "git-annex" configdir)

	ifM isRoot
		( return ()
		, do
			programfile <- inDestDir =<< programFile
			createDirectoryIfMissing True (parentDir programfile)
			writeFile programfile command
		)

writeOSXDesktop :: FilePath -> IO ()
writeOSXDesktop command = do
	home <- myHomeDir

	let base = "Library" </> "LaunchAgents" </> label ++ ".plist"
	autostart <- ifM isRoot ( inDestDir $ "/" </> base , inDestDir $ home </> base)
	createDirectoryIfMissing True (parentDir autostart)
	writeFile autostart $ genOSXAutoStartFile label command

	let appdir = "git-annex.app"
	installOSXAppFile appdir "Contents/Info.plist" Nothing
	installOSXAppFile appdir "Contents/Resources/git-annex.icns" Nothing
	installOSXAppFile appdir "Contents/MacOS/git-annex" (Just webappscript)
	where
		label = "com.branchable.git-annex.assistant"
		webappscript = unlines
			[ "#!/bin/sh"
			, command ++ " webapp"
			]

installOSXAppFile :: FilePath -> FilePath -> Maybe String -> IO ()
installOSXAppFile appdir appfile mcontent = do
	let src = "ui-macos" </> appdir </> appfile
	home <- myHomeDir
	dest <- ifM isRoot
		( return $ "/Applications" </> appdir </> appfile
		, return $ home </> "Desktop" </> appdir </> appfile
		)
	content <- maybe (readFile src) return mcontent
	createDirectoryIfMissing True (parentDir dest)
	writeFile dest content
	mode <- fileMode <$> getFileStatus src
	setFileMode dest mode

genOSXAutoStartFile :: String -> String -> String
genOSXAutoStartFile label command = unlines
	[ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	, "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
	, "<plist version=\"1.0\">"
	, "<dict>"
	, "<key>Label</key>"
	, "<string>" ++ label ++ "</string>"
	, "<key>ProgramArguments</key>"
	, "<array>"
	, "<string>" ++ command ++ "</string>"
	, "<string>assistant</string>"
	, "<string>--autostart</string>"
	, "</array>"
	, "<key>RunAtLoad</key>"
	, "</dict>"
	, "</plist>"
	]

writeDesktop :: FilePath -> IO ()
#ifdef darwin_HOST_OS
writeDesktop = writeOSXDesktop
#else
writeDesktop = writeFDODesktop
#endif

main = getArgs >>= go
	where
		go [] = error "specify git-annex command"
		go (command:_) = writeDesktop command

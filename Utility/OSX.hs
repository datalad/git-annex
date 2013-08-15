{- OSX stuff
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.OSX where

import Utility.UserInfo

import System.FilePath

autoStartBase :: String -> FilePath
autoStartBase label = "Library" </> "LaunchAgents" </> label ++ ".plist"

systemAutoStart :: String -> FilePath
systemAutoStart label = "/" </> autoStartBase label

userAutoStart :: String -> IO FilePath
userAutoStart label = do
	home <- myHomeDir
	return $ home </> autoStartBase label

{- Generates an OSX autostart plist file with a given label, command, and
 - params to run at boot or login. -}
genOSXAutoStartFile :: String -> String -> [String] -> String
genOSXAutoStartFile label command params = unlines
	[ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	, "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
	, "<plist version=\"1.0\">"
	, "<dict>"
	, "<key>Label</key>"
	, "<string>" ++ label ++ "</string>"
	, "<key>ProgramArguments</key>"
	, "<array>"
	, unlines $ map (\v -> "<string>" ++ v ++ "</string>") (command:params)
	, "</array>"
	, "<key>RunAtLoad</key>"
	, "<true/>"
	, "</dict>"
	, "</plist>"
	]
	

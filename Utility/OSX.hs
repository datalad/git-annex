{- OSX stuff
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OSX (
	autoStartBase,
	systemAutoStart,
	userAutoStart,
	genOSXAutoStartFile,
) where

import Common
import Utility.UserInfo

autoStartBase :: String -> OsPath
autoStartBase label = literalOsPath "Library" 
	</> literalOsPath "LaunchAgents"
	</> toOsPath label <> literalOsPath ".plist"

systemAutoStart :: String -> OsPath
systemAutoStart label = literalOsPath "/" </> autoStartBase label

userAutoStart :: String -> IO OsPath
userAutoStart label = do
	home <- myHomeDir
	return $ toOsPath home </> autoStartBase label

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
	

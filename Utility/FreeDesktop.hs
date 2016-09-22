{- Freedesktop.org specifications
 -
 - http://standards.freedesktop.org/basedir-spec/latest/
 - http://standards.freedesktop.org/desktop-entry-spec/latest/
 - http://standards.freedesktop.org/menu-spec/latest/
 - http://standards.freedesktop.org/icon-theme-spec/latest/
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.FreeDesktop (
	DesktopEntry,	
	genDesktopEntry,
	buildDesktopMenuFile,
	writeDesktopMenuFile,
	desktopMenuFilePath,
	autoStartPath,
	iconDir,
	iconFilePath,
	systemDataDir,
	systemConfigDir,
	userDataDir,
	userConfigDir,
	userDesktopDir
) where

import Utility.Exception
import Utility.UserInfo
import Utility.Process
import Utility.PartialPrelude
import Utility.Directory

import System.Environment
import System.FilePath
import Data.List
import Data.Maybe
import Control.Applicative
import Prelude

type DesktopEntry = [(Key, Value)]

type Key = String

data Value = StringV String | BoolV Bool | NumericV Float | ListV [Value]

toString :: Value -> String
toString (StringV s) = s
toString (BoolV b)
	| b = "true"
	| otherwise = "false"
toString (NumericV f) = show f
toString (ListV l)
	| null l = ""
	| otherwise = (intercalate ";" $ map (concatMap escapesemi . toString) l) ++ ";"
  where
	escapesemi ';' = "\\;"
	escapesemi c = [c]

genDesktopEntry :: String -> String -> Bool -> FilePath -> Maybe String -> [String] -> DesktopEntry
genDesktopEntry name comment terminal program icon categories = catMaybes
	[ item "Type" StringV "Application"
	, item "Version" NumericV 1.0
	, item "Name" StringV name
	, item "Comment" StringV comment
	, item "Terminal" BoolV terminal
	, item "Exec" StringV program
	, maybe Nothing (item "Icon" StringV) icon
	, item "Categories" ListV (map StringV categories)
	]
  where
	item x c y = Just (x, c y)

buildDesktopMenuFile :: DesktopEntry -> String
buildDesktopMenuFile d = unlines ("[Desktop Entry]" : map keyvalue d) ++ "\n"
  where
	keyvalue (k, v) = k ++ "=" ++ toString v

writeDesktopMenuFile :: DesktopEntry -> String -> IO ()
writeDesktopMenuFile d file = do
	createDirectoryIfMissing True (takeDirectory file)
	writeFile file $ buildDesktopMenuFile d

{- Path to use for a desktop menu file, in either the systemDataDir or
 - the userDataDir -}
desktopMenuFilePath :: String -> FilePath -> FilePath
desktopMenuFilePath basename datadir = 
	datadir </> "applications" </> desktopfile basename

{- Path to use for a desktop autostart file, in either the systemDataDir
 - or the userDataDir -}
autoStartPath :: String -> FilePath -> FilePath
autoStartPath basename configdir =
	configdir </> "autostart" </> desktopfile basename

{- Base directory to install an icon file, in either the systemDataDir
 - or the userDatadir. -}
iconDir :: FilePath -> FilePath
iconDir datadir = datadir </> "icons" </> "hicolor"

{- Filename of an icon, given the iconDir to use.
 -
 - The resolution is something like "48x48" or "scalable". -}
iconFilePath :: FilePath -> String -> FilePath -> FilePath
iconFilePath file resolution icondir =
	icondir </> resolution </> "apps" </> file

desktopfile :: FilePath -> FilePath
desktopfile f = f ++ ".desktop"

{- Directory used for installation of system wide data files.. -}
systemDataDir :: FilePath
systemDataDir = "/usr/share"

{- Directory used for installation of system wide config files. -}
systemConfigDir :: FilePath
systemConfigDir = "/etc/xdg"

{- Directory for user data files. -}
userDataDir :: IO FilePath
userDataDir = xdgEnvHome "DATA_HOME" ".local/share"

{- Directory for user config files. -}
userConfigDir :: IO FilePath
userConfigDir = xdgEnvHome "CONFIG_HOME" ".config"

{- Directory for the user's Desktop, may be localized. 
 -
 - This is not looked up very fast; the config file is in a shell format
 - that is best parsed by shell, so xdg-user-dir is used, with a fallback
 - to ~/Desktop. -}
userDesktopDir :: IO FilePath
userDesktopDir = maybe fallback return =<< (parse <$> xdg_user_dir)
  where
	parse = maybe Nothing (headMaybe . lines)
	xdg_user_dir = catchMaybeIO $ readProcess "xdg-user-dir" ["DESKTOP"]
	fallback = xdgEnvHome "DESKTOP_DIR" "Desktop"

xdgEnvHome :: String -> String -> IO String
xdgEnvHome envbase homedef = do
	home <- myHomeDir
	catchDefaultIO (home </> homedef) $
		getEnv $ "XDG_" ++ envbase

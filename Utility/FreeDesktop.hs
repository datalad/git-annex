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

import Common
import Utility.UserInfo

import System.Environment

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

writeDesktopMenuFile :: DesktopEntry -> OsPath -> IO ()
writeDesktopMenuFile d file = do
	createDirectoryIfMissing True (takeDirectory file)
	writeFile (fromOsPath file) $ buildDesktopMenuFile d

{- Path to use for a desktop menu file, in either the systemDataDir or
 - the userDataDir -}
desktopMenuFilePath :: String -> OsPath -> OsPath
desktopMenuFilePath basename datadir = 
	datadir </> literalOsPath "applications" </> desktopfile basename

{- Path to use for a desktop autostart file, in either the systemDataDir
 - or the userDataDir -}
autoStartPath :: String -> OsPath -> OsPath
autoStartPath basename configdir =
	configdir </> literalOsPath "autostart" </> desktopfile basename

{- Base directory to install an icon file, in either the systemDataDir
 - or the userDatadir. -}
iconDir :: OsPath -> OsPath
iconDir datadir = datadir </> literalOsPath "icons" </> literalOsPath "hicolor"

{- Filename of an icon, given the iconDir to use.
 -
 - The resolution is something like "48x48" or "scalable". -}
iconFilePath :: OsPath -> String -> OsPath -> OsPath
iconFilePath file resolution icondir =
	icondir </> toOsPath resolution </> literalOsPath "apps" </> file

desktopfile :: FilePath -> OsPath
desktopfile f = toOsPath $ f ++ ".desktop"

{- Directory used for installation of system wide data files.. -}
systemDataDir :: OsPath
systemDataDir = literalOsPath "/usr/share"

{- Directory used for installation of system wide config files. -}
systemConfigDir :: OsPath
systemConfigDir = literalOsPath "/etc/xdg"

{- Directory for user data files. -}
userDataDir :: IO OsPath
userDataDir = toOsPath <$> xdgEnvHome "DATA_HOME" ".local/share"

{- Directory for user config files. -}
userConfigDir :: IO OsPath
userConfigDir = toOsPath <$> xdgEnvHome "CONFIG_HOME" ".config"

{- Directory for the user's Desktop, may be localized. 
 -
 - This is not looked up very fast; the config file is in a shell format
 - that is best parsed by shell, so xdg-user-dir is used, with a fallback
 - to ~/Desktop. -}
userDesktopDir :: IO FilePath
userDesktopDir = maybe fallback return =<< (parse <$> xdg_user_dir)
  where
	parse s = case lines <$> s of
		Just (l:_) -> Just l
		_ -> Nothing
	xdg_user_dir = catchMaybeIO $ readProcess "xdg-user-dir" ["DESKTOP"]
	fallback = xdgEnvHome "DESKTOP_DIR" "Desktop"

xdgEnvHome :: String -> String -> IO String
xdgEnvHome envbase homedef = do
	home <- toOsPath <$> myHomeDir
	catchDefaultIO (fromOsPath $ home </> toOsPath homedef) $
		getEnv ("XDG_" ++ envbase)

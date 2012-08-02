{- Freedesktop.org specifications
 -
 - http://standards.freedesktop.org/basedir-spec/latest/
 - http://standards.freedesktop.org/desktop-entry-spec/latest/
 - http://standards.freedesktop.org/menu-spec/latest/
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FreeDesktop (
	DesktopEntry,	
	genDesktopEntry,
	buildDesktopMenuFile,
	writeDesktopMenuFile,
	desktopMenuFilePath,
	autoStartPath,
	systemDataDir,
	systemConfigDir,
	userDataDir,
	userConfigDir	
) where

import Utility.Exception
import Utility.Directory
import Utility.Path

import System.IO
import System.Environment
import System.Directory
import System.FilePath
import Data.List
import Data.String.Utils

type DesktopEntry = [(Key, Value)]

type Key = String

data Value = StringV String | BoolV Bool | NumericV Float | ListV [Value]

toString :: Value -> String
toString (StringV s) = s
toString (BoolV b)
	| b = "true"
	| otherwise = "false"
toString(NumericV f) = show f
toString (ListV l)
	| null l = ""
	| otherwise = (intercalate ";" $ map (escapesemi . toString) l) ++ ";"
	where
		escapesemi = join "\\;" . split ";"

genDesktopEntry :: String -> String -> Bool -> FilePath -> [String] -> DesktopEntry
genDesktopEntry name comment terminal program categories =
	[ item "Type" StringV "Application"
	, item "Version" NumericV 1.0
	, item "Name" StringV name
	, item "Comment" StringV comment
	, item "Terminal" BoolV terminal
	, item "Exec" StringV program
	, item "Categories" ListV (map StringV categories)
	]
	where
		item x c y = (x, c y)

buildDesktopMenuFile :: DesktopEntry -> String
buildDesktopMenuFile d = unlines ("[Desktop Entry]" : map keyvalue d) ++ "\n"
	where
		keyvalue (k, v) = k ++ "=" ++ toString v

writeDesktopMenuFile :: DesktopEntry -> String -> IO ()
writeDesktopMenuFile d file = do
	createDirectoryIfMissing True (parentDir file)
	writeFile file $ buildDesktopMenuFile d

desktopMenuFilePath :: String -> FilePath -> FilePath
desktopMenuFilePath basename datadir = 
	datadir </> "applications" </> desktopfile basename

autoStartPath :: String -> FilePath -> FilePath
autoStartPath basename configdir =
	configdir </> "autostart" </> desktopfile basename

desktopfile :: FilePath -> FilePath
desktopfile f = f ++ ".desktop"

systemDataDir :: FilePath
systemDataDir = "/usr/share"

systemConfigDir :: FilePath
systemConfigDir = "/etc/xdg"

userDataDir :: IO FilePath
userDataDir = do
	dir <- xdgEnv "DATA_HOME" =<< myHomeDir
	return $ dir </> ".local" </> "share"

userConfigDir :: IO FilePath
userConfigDir = do
	dir <- xdgEnv "CONFIG_HOME" =<< myHomeDir
	return $ dir </> ".config"

xdgEnv :: String -> String -> IO String
xdgEnv envbase def = catchDefaultIO (getEnv $ "XDG_" ++ envbase) def

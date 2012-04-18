{- git-annex remote log
 - 
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 - 
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Remote (
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,

	prop_idempotent_configEscape
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Data.Char

import Common.Annex
import qualified Annex.Branch
import Types.Remote
import Logs.UUIDBased

{- Filename of remote.log. -}
remoteLog :: FilePath
remoteLog = "remote.log"

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u c = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change remoteLog $
		showLog showConfig . changeLog ts u c . parseLog parseConfig

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = (simpleMap . parseLog parseConfig) <$> Annex.Branch.get remoteLog

parseConfig :: String -> Maybe RemoteConfig
parseConfig = Just . keyValToConfig . words

showConfig :: RemoteConfig -> String
showConfig = unwords . configToKeyVal

{- Given Strings like "key=value", generates a RemoteConfig. -}
keyValToConfig :: [String] -> RemoteConfig
keyValToConfig ws = M.fromList $ map (/=/) ws
	where
		(/=/) s = (k, v)
			where
				k = takeWhile (/= '=') s
				v = configUnEscape $ drop (1 + length k) s

configToKeyVal :: M.Map String String -> [String]
configToKeyVal m = map toword $ sort $ M.toList m
	where
		toword (k, v) = k ++ "=" ++ configEscape v

configEscape :: String -> String
configEscape = (>>= escape)
	where
		escape c
			| isSpace c || c `elem` "&" = "&" ++ show (ord c) ++ ";"
			| otherwise = [c]

configUnEscape :: String -> String
configUnEscape = unescape
	where
		unescape [] = []
		unescape (c:rest)
			| c == '&' = entity rest
			| otherwise = c : unescape rest
		entity s
			| not (null num) && ";" `isPrefixOf` r =
				chr (Prelude.read num) : unescape rest
			| otherwise =
				'&' : unescape s
			where
				num = takeWhile isNumber s
				r = drop (length num) s
				rest = drop 1 r

{- for quickcheck -}
prop_idempotent_configEscape :: String -> Bool
prop_idempotent_configEscape s = s == (configUnEscape . configEscape) s

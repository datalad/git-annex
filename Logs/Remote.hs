{- git-annex remote log
 - 
 - Copyright 2011 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Remote (
	remoteLog,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,
	showConfig,
	parseConfig,

	prop_isomorphic_configEscape,
	prop_parse_show_Config,
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Data.Char

import Common.Annex
import qualified Annex.Branch
import Types.Remote
import Logs
import Logs.UUIDBased

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u c = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change remoteLog $
		showLog showConfig . changeLog ts u c . parseLog parseConfig

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = simpleMap . parseLog parseConfig <$> Annex.Branch.get remoteLog

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
configEscape = concatMap escape
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
prop_isomorphic_configEscape :: String -> Bool
prop_isomorphic_configEscape s = s == (configUnEscape . configEscape) s

prop_parse_show_Config :: RemoteConfig -> Bool
prop_parse_show_Config c
	-- whitespace and '=' are not supported in keys
	| any (\k -> any isSpace k || elem '=' k) (M.keys c) = True
	| otherwise = parseConfig (showConfig c) ~~ Just c
  where
	normalize v = sort . M.toList <$> v
	a ~~ b = normalize a == normalize b

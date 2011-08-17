{- git-annex remote log
 - 
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 - 
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteLog (
	remoteLog,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,

	prop_idempotent_configEscape
) where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char

import qualified Branch
import Types
import Types.Remote
import UUID

{- Filename of remote.log. -}
remoteLog :: FilePath
remoteLog = "remote.log"

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u c = do
	m <- readRemoteLog
	Branch.change remoteLog $ unlines $ sort $
		map toline $ M.toList $ M.insert u c m
	where
		toline (u', c') = u' ++ " " ++ unwords (configToKeyVal c')

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = return . remoteLogParse =<< Branch.get remoteLog

remoteLogParse :: String -> M.Map UUID RemoteConfig
remoteLogParse s =
	M.fromList $ mapMaybe parseline $ filter (not . null) $ lines s
	where
		parseline l
			| length w > 2 = Just (u, c)
			| otherwise = Nothing
			where
				w = words l
				u = head w
				c = keyValToConfig $ tail w

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
		entity s = if ok
				then chr (read num) : unescape rest
				else '&' : unescape s
			where
				num = takeWhile isNumber s
				r = drop (length num) s
				rest = drop 1 r
				ok = not (null num) && 
					not (null r) && head r == ';'

{- for quickcheck -}
prop_idempotent_configEscape :: String -> Bool
prop_idempotent_configEscape s = s == (configUnEscape . configEscape) s

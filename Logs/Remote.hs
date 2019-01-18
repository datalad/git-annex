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

	prop_isomorphic_configEscape,
	prop_parse_show_Config,
) where

import Annex.Common
import qualified Annex.Branch
import Types.Remote
import Logs
import Logs.UUIDBased

import qualified Data.Map as M
import Data.Char
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u cfg = do
	c <- liftIO currentVectorClock
	Annex.Branch.change remoteLog $
		buildLog (byteString . encodeBS . showConfig)
			. changeLog c u cfg
			. parseLog remoteConfigParser

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = simpleMap . parseLog remoteConfigParser
	<$> Annex.Branch.get remoteLog

remoteConfigParser :: A.Parser RemoteConfig
remoteConfigParser = keyValToConfig . words . decodeBS <$> A.takeByteString

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
	-- whitespace and '=' are not supported in config keys; limit to
	-- alphanumerics for simplicity
	| any (all isAlphaNum) (M.keys c) = True
	| otherwise = A.parseOnly remoteConfigParser (encodeBS $ showConfig c) ~~ Right c
  where
	normalize v = sort . M.toList <$> v
	a ~~ b = normalize a == normalize b

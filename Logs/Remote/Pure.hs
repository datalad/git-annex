{- git-annex remote log, pure operations
 - 
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Remote.Pure (
	calcRemoteConfigMap,
	parseRemoteConfigLog,
	buildRemoteConfigLog,
        keyValToConfig,
        configToKeyVal,
        showConfig,

        prop_isomorphic_configEscape,
        prop_parse_show_Config,
) where

import Annex.Common
import Types.Remote
import Types.ProposedAccepted
import Logs.UUIDBased
import Annex.SpecialRemote.Config
import Utility.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Char
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString.Builder

calcRemoteConfigMap :: L.ByteString -> M.Map UUID RemoteConfig
calcRemoteConfigMap = (\m -> M.map (addSameasInherited m) m)
	. simpleMap
	. parseRemoteConfigLog

parseRemoteConfigLog :: L.ByteString -> Log RemoteConfig
parseRemoteConfigLog = parseLogOld remoteConfigParser

buildRemoteConfigLog :: Log RemoteConfig -> Builder
buildRemoteConfigLog = buildLogOld (byteString . encodeBS . showConfig)

remoteConfigParser :: A.Parser RemoteConfig
remoteConfigParser = keyValToConfig Accepted . words . decodeBS <$> A.takeByteString

showConfig :: RemoteConfig -> String
showConfig = unwords . configToKeyVal

{- Given Strings like "key=value", generates a RemoteConfig. -}
keyValToConfig :: (String -> ProposedAccepted String) -> [String] -> RemoteConfig
keyValToConfig mk ws = M.fromList $ map (/=/) ws
  where
	(/=/) s = (mk k, mk v)
	  where
		k = takeWhile (/= '=') s
		v = configUnEscape $ drop (1 + length k) s

configToKeyVal :: RemoteConfig -> [String]
configToKeyVal m = map toword $ sort $ M.toList m
  where
	toword (k, v) = fromProposedAccepted k ++ "=" ++ configEscape (fromProposedAccepted v)

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
prop_isomorphic_configEscape :: TestableString -> Bool
prop_isomorphic_configEscape ts = s == (configUnEscape . configEscape) s
  where
	s = fromTestableString ts

prop_parse_show_Config :: RemoteConfig -> Bool
prop_parse_show_Config c
	-- whitespace and '=' are not supported in config keys
	| any (\k -> any isSpace k || elem '=' k) (map fromProposedAccepted $ M.keys c) = True
	| any (any excluded) (map fromProposedAccepted $ M.keys c) = True
	| any (any excluded) (map fromProposedAccepted $ M.elems c) = True
	| otherwise = A.parseOnly remoteConfigParser (encodeBS $ showConfig c) ~~ Right c
  where
	normalize v = sort . M.toList <$> v
	a ~~ b = normalize a == normalize b
	-- limit to ascii alphanumerics for simplicity; characters not
	-- allowed by the current character set in the config may not
	-- round-trip in an identical representation due to the use of the
	-- filesystem encoding.
	excluded ch = not (isAlphaNum ch) || not (isAscii ch)

{- P2P protocol over HTTP, urls
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module P2P.Http.Url where

import Data.List
import Network.URI
import System.FilePath.Posix as P
import Servant.Client (BaseUrl(..), Scheme(..))
import Text.Read
import Data.Char
import qualified Git
import qualified Git.Url

defaultP2PHttpProtocolPort :: Int
defaultP2PHttpProtocolPort = 9417 -- Git protocol is 9418

isP2PHttpProtocolUrl :: String -> Bool
isP2PHttpProtocolUrl s = 
	"annex+http://" `isPrefixOf` s ||
	"annex+https://" `isPrefixOf` s

data P2PHttpUrl = P2PHttpUrl
	{ p2pHttpUrlString :: String
	, p2pHttpBaseUrl :: BaseUrl
	}
	deriving (Show)

parseP2PHttpUrl :: String -> Maybe P2PHttpUrl
parseP2PHttpUrl us
	| isP2PHttpProtocolUrl us = case parseURI (drop prefixlen us) of
		Nothing -> Nothing
		Just u ->
			case uriScheme u of
				"http:" -> mkbaseurl Http u
				"https:" -> mkbaseurl Https u
				_ -> Nothing
	| otherwise = Nothing
  where
	prefixlen = length "annex+"

	mkbaseurl s u = do
		auth <- uriAuthority u
		port <- if null (uriPort auth)
			then Just defaultP2PHttpProtocolPort
			else readMaybe (dropWhile (== ':') (uriPort auth))
		return $ P2PHttpUrl us $ BaseUrl
			{ baseUrlScheme = s
			, baseUrlHost = uriRegName auth
			, baseUrlPath = basepath u
			, baseUrlPort = port
			}
	
	-- The servant server uses urls that start with "/git-annex/",
	-- and so the servant client adds that to the base url. So remove
	-- it from the url that the user provided. However, it may not be
	-- present, eg if some other server is speaking the git-annex
	-- protocol. The UUID is also removed from the end of the url.
	basepath u = case reverse $ P.splitDirectories (uriPath u) of
		("git-annex":"/":rest) -> P.joinPath (reverse rest)
		rest -> P.joinPath (reverse rest)

unavailableP2PHttpUrl :: P2PHttpUrl -> P2PHttpUrl
unavailableP2PHttpUrl p = p
	{ p2pHttpBaseUrl = (p2pHttpBaseUrl p) { baseUrlHost = "!dne!" } }

-- When a p2phttp url is on the same host as a git repo, which also uses
-- http, the same username+password is assumed to be used for both.
isP2PHttpSameHost :: P2PHttpUrl -> Git.Repo -> Bool
isP2PHttpSameHost u repo
	| not (Git.repoIsHttp repo) = False
	| otherwise = 
		Just (map toLower $ baseUrlHost (p2pHttpBaseUrl u)) 
			==
		(map toLower <$> (Git.Url.host repo))

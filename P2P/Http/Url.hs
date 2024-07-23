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
#ifdef WITH_SERVANT
import Servant.Client (BaseUrl(..), Scheme(..))
import Text.Read
#endif

defaultP2PHttpProtocolPort :: Int
defaultP2PHttpProtocolPort = 9417 -- Git protocol is 9418

isP2PHttpProtocolUrl :: String -> Bool
isP2PHttpProtocolUrl s = 
	"annex+http://" `isPrefixOf` s ||
	"annex+https://" `isPrefixOf` s

data P2PHttpUrl =
#ifdef WITH_SERVANT
	P2PHttpUrl String BaseUrl
#else
	P2PHttpUrl String
#endif

parseP2PHttpUrl :: String -> Maybe P2PHttpUrl
parseP2PHttpUrl us
	| isP2PHttpProtocolUrl us = case parseURI (drop prefixlen us) of
		Nothing -> Nothing
#ifdef WITH_SERVANT
		Just u -> 
			case uriScheme u of
				"http:" -> mkbaseurl Http u
				"https:" -> mkbaseurl Https u
				_ -> Nothing
#else
		Just _ -> 
			Just (P2PHttpUrl us)
#endif
	| otherwise = Nothing
  where
	prefixlen = length "annex+"
#ifdef WITH_SERVANT
	mkbaseurl s u = do
		auth <- uriAuthority u
		port <- if null (uriPort auth)
			then Just defaultP2PHttpProtocolPort
			else readMaybe (dropWhile (== ':') (uriPort auth))
		return $ P2PHttpUrl us $ BaseUrl
			{ baseUrlScheme = s
			, baseUrlHost = uriRegName auth
			, baseUrlPath = uriPath u
			, baseUrlPort = port
			}
#endif

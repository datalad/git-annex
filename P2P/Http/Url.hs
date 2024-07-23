{- P2P protocol over HTTP, urls
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module P2P.Http.Url where

import Types.UUID
import Utility.FileSystemEncoding
import Utility.PartialPrelude

import Data.List
import Network.URI
import System.FilePath.Posix as P
import qualified Data.UUID as UUID
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

data P2PHttpUrl = P2PHttpUrl
	{ p2pHttpUrlString :: String
	, p2pHttpUUID :: Maybe UUID
#ifdef WITH_SERVANT
	, p2pHttpBaseUrl :: BaseUrl
#endif
	}
	deriving (Show)

parseP2PHttpUrl :: String -> Maybe P2PHttpUrl
parseP2PHttpUrl us
	| isP2PHttpProtocolUrl us = case parseURI (drop prefixlen us) of
		Nothing -> Nothing
		Just u ->
#ifdef WITH_SERVANT
			case uriScheme u of
				"http:" -> mkbaseurl Http u
				"https:" -> mkbaseurl Https u
				_ -> Nothing
#else
			Just $ P2PHttpUrl us (extractuuid u)
#endif
	| otherwise = Nothing
  where
	prefixlen = length "annex+"

	extractuuid u = do
		p <- lastMaybe $ P.splitDirectories (uriPath u)
		-- While git-annex generally allows a UUID that is not
		-- well formed, here it's important to make sure that the
		-- url a user provided really ends with a UUID, so check
		-- that it's well formed.
		case UUID.fromString p of
			Nothing -> Nothing
			Just _ -> return (UUID (encodeBS p)) 

#ifdef WITH_SERVANT
	mkbaseurl s u = do
		auth <- uriAuthority u
		port <- if null (uriPort auth)
			then Just defaultP2PHttpProtocolPort
			else readMaybe (dropWhile (== ':') (uriPort auth))
		return $ P2PHttpUrl us (extractuuid u) $ BaseUrl
			{ baseUrlScheme = s
			, baseUrlHost = uriRegName auth
			, baseUrlPath = uriPath u
			, baseUrlPort = port
			}
#endif

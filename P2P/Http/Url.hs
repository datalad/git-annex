{- P2P protocol over HTTP, urls
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module P2P.Http.Url where

import Data.List

defaultP2PHttpProtocolPort :: Int
defaultP2PHttpProtocolPort = 9417 -- Git protocol is 9418

isP2PHttpProtocolUrl :: String -> Bool
isP2PHttpProtocolUrl s = 
	"annex+http://" `isPrefixOf` s ||
	"annex+https://" `isPrefixOf` s

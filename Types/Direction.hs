{- git-annex transfer direction types
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Types.Direction where

import qualified Data.ByteString as B

data Direction = Upload | Download
	deriving (Eq, Ord, Show, Read)

formatDirection :: Direction -> B.ByteString
formatDirection Upload = "upload"
formatDirection Download = "download"

parseDirection :: B.ByteString -> Maybe Direction
parseDirection "upload" = Just Upload
parseDirection "download" = Just Download
parseDirection _ = Nothing


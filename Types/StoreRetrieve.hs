{- Types for Storer and Retriever actions for remotes.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE Rank2Types #-}

module Types.StoreRetrieve where

import Common.Annex
import Utility.Metered

import qualified Data.ByteString.Lazy as L

-- Prepares for and then runs an action that will act on a Key's
-- content, passing it a helper when the preparation is successful.
type Preparer helper = forall a. Key -> (Maybe helper -> Annex a) -> Annex a

-- A source of a Key's content.
data ContentSource
	= FileContent FilePath
	| ByteContent L.ByteString

isByteContent :: ContentSource -> Bool
isByteContent (ByteContent _) = True
isByteContent (FileContent _) = False

-- Action that stores a Key's content on a remote.
-- Can throw exceptions.
type Storer = Key -> ContentSource -> MeterUpdate -> Annex Bool

-- Action that retrieves a Key's content from a remote, passing it to a
-- callback.
-- Throws exception if key is not present, or remote is not accessible.
type Retriever = Key -> MeterUpdate -> (ContentSource -> Annex Bool) -> Annex Bool

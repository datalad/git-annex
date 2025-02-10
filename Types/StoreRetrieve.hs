{- Types for Storer and Retriever actions for remotes.
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Types.StoreRetrieve where

import Annex.Common
import Types.NumCopies
import Utility.Metered
import Utility.Hash (IncrementalVerifier)

import qualified Data.ByteString.Lazy as L

-- A source of a Key's content.
data ContentSource
	= FileContent OsPath
	| ByteContent L.ByteString

isByteContent :: ContentSource -> Bool
isByteContent (ByteContent _) = True
isByteContent (FileContent _) = False

-- Action that stores a Key's content on a remote.
-- Can throw exceptions.
type Storer = Key -> ContentSource -> MeterUpdate -> Annex ()

-- Action that retrieves a Key's content from a remote, passing it to a
-- callback, which will fully consume the content before returning.
--
-- Throws exception if key is not present, or remote is not accessible.
--
-- When it retrieves FileContent, it is responsible for updating the
-- MeterUpdate, and the provided FilePath can be used to store the file
-- it retrieves. 
--
-- When the IncrementalVerifier is passed to it,
-- and it retrieves FileContent, it can feed some or all of the file's
-- content to the verifier before running the callback.
-- This should not be done when it retrieves ByteContent.
type Retriever = forall a.
	Key -> MeterUpdate -> OsPath -> Maybe IncrementalVerifier
		-> (ContentSource -> Annex a) -> Annex a

-- Action that removes a Key's content from a remote.
-- Succeeds if key is already not present.
-- Throws an exception if the remote is not accessible
-- or the proof has expired.
type Remover = Maybe SafeDropProof -> Key -> Annex ()

-- Checks if a Key's content is present on a remote.
-- Throws an exception if the remote is not accessible.
type CheckPresent = Key -> Annex Bool

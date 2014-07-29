{- Types for Storer and Retriever
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE Rank2Types #-}

module Types.StoreRetrieve where

import Common.Annex
import Utility.Metered
import Utility.Tmp

import qualified Data.ByteString.Lazy as L

-- Prepares for and then runs an action that will act on a Key's
-- content, passing it a helper when the preparation is successful.
type Preparer helper = forall a. Key -> (Maybe helper -> Annex a) -> Annex a

-- A source of a Key's content.
data ContentSource
	= FileContent FilePath
	| ByteContent L.ByteString

-- Action that stores a Key's content on a remote.
-- Can throw exceptions.
type Storer = Key -> ContentSource -> MeterUpdate -> IO Bool

-- Action that retrieves a Key's content from a remote.
-- Throws exception if key is not present, or remote is not accessible.
type Retriever = Key -> IO ContentSource

fileStorer :: (Key -> FilePath -> MeterUpdate -> IO Bool) -> Storer
fileStorer a k (FileContent f) m = a k f m
fileStorer a k (ByteContent b) m = do
	 withTmpFile "tmpXXXXXX" $ \f h -> do
		L.hPut h b
		hClose h
		a k f m

byteStorer :: (Key -> L.ByteString -> MeterUpdate -> IO Bool) -> Storer
byteStorer a k c m = withBytes c $ \b -> a k b m

withBytes :: ContentSource -> (L.ByteString -> IO a) -> IO a
withBytes (ByteContent b) a = a b
withBytes (FileContent f) a = a =<< L.readFile f

fileRetriever :: (Key -> IO FilePath) -> Retriever
fileRetriever a k = FileContent <$> a k

byteRetriever :: (Key -> IO L.ByteString) -> Retriever
byteRetriever a k = ByteContent <$> a k

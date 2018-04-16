{- GHC File system encoding support for Aeson.
 -
 - Import instead of Data.Aeson
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Utility.Aeson (
	module X,
	ToJSON'(..),
	encode,
	packString,
) where

import Data.Aeson as X hiding (ToJSON, toJSON, encode)
import Data.Aeson hiding (encode)
import qualified Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Set
import qualified Data.Vector
import Prelude

import Utility.FileSystemEncoding

-- | Use this instead of Data.Aeson.encode to make sure that the
-- below String instance is used.
encode :: ToJSON' a => a -> L.ByteString
encode = Data.Aeson.encode . toJSON'

-- | Aeson has an unfortunate ToJSON instance for Char and [Char]
-- which does not support Strings containing UTF8 characters
-- encoded using the filesystem encoding when run in a non-utf8 locale.
--
-- Since we can't replace that with a instance that does the right
-- thing, instead here's a new class that handles String right.
class ToJSON' a where
	toJSON' :: a -> Value

instance ToJSON' String where
	toJSON' = toJSON . packString

-- | Pack a String to Text, correctly handling the filesystem encoding.
--
-- Use this instead of Data.Text.pack.
--
-- Note that if the string contains invalid UTF8 characters not using
-- the FileSystemEncoding, this is the same as Data.Text.pack.
packString :: String -> T.Text
packString s = case T.decodeUtf8' (S.concat $ L.toChunks $ encodeBS s) of
	Right t -> t
	Left _ -> T.pack s

-- | An instance for lists cannot be included as it would overlap with
-- the String instance. Instead, you can use a Vector.
instance ToJSON' s => ToJSON' (Data.Vector.Vector s) where
	toJSON' = toJSON . map toJSON' . Data.Vector.toList

-- Aeson generates the same JSON for a Set as for a list.
instance ToJSON' s => ToJSON' (Data.Set.Set s) where
	toJSON' = toJSON . map toJSON' . Data.Set.toList

instance (ToJSON' a, ToJSON a) => ToJSON' (Maybe a) where
	toJSON' (Just a) = toJSON (Just (toJSON' a))
	toJSON' v@Nothing = toJSON v

instance (ToJSON' a, ToJSON a, ToJSON' b, ToJSON b) => ToJSON' (a, b) where
	toJSON' (a, b) = toJSON ((toJSON' a, toJSON' b))

instance ToJSON' Bool where
	toJSON' = toJSON

instance ToJSON' Integer where
	toJSON' = toJSON

instance ToJSON' Object where
	toJSON' = toJSON

instance ToJSON' Value where
	toJSON' = toJSON

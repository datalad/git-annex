{- Past and present tense text.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utility.Tense where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Exts( IsString(..) )

data Tense = Present | Past
	deriving (Eq)

data TenseChunk = Tensed Text Text | UnTensed Text
	deriving (Eq, Ord, Show)

newtype TenseText = TenseText [TenseChunk]
	deriving (Eq, Ord)

{- Allows OverloadedStrings to be used, to build UnTensed chunks. -}
instance IsString TenseChunk where
	fromString = UnTensed . T.pack

{- Allows OverloadedStrings to be used, to provide UnTensed TenseText. -}
instance IsString TenseText where
	fromString s = TenseText [fromString s]

renderTense :: Tense -> TenseText -> Text
renderTense tense (TenseText chunks) = T.concat $ map render chunks
  where
	render (Tensed present past)
		| tense == Present = present
		| otherwise = past
	render (UnTensed s) = s

{- Builds up a TenseText, separating chunks with spaces.
 -
 - However, rather than just intersperse new chunks for the spaces,
 - the spaces are appended to the end of the chunks.
 -}
tenseWords :: [TenseChunk] -> TenseText
tenseWords = TenseText . go []
  where
	go c [] = reverse c
	go c (w:[]) = reverse (w:c)
	go c ((UnTensed w):ws) = go (UnTensed (addspace w) : c) ws
	go c ((Tensed w1 w2):ws) =
		go (Tensed (addspace w1) (addspace w2) : c) ws
	addspace w = T.append w " "

unTensed :: Text -> TenseText
unTensed t = TenseText [UnTensed t]

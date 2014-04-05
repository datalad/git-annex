{- Simple line-based protocols.
 -
 - Copyright 2013-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.SimpleProtocol (
	Sendable(..),
	Receivable(..),
	parseMessage,
	Serializable(..),
	Parser,
	parseFail,
	parse0,
	parse1,
	parse2,
	parse3,
) where

import Control.Applicative
import Data.Char

import Utility.Misc

-- Messages that can be sent.
class Sendable m where
	formatMessage :: m -> [String]

-- Messages that can be received.
class Receivable m where
	-- Passed the first word of the message, returns
	-- a Parser that can be be fed the rest of the message to generate
	-- the value.
	parseCommand :: String -> Parser m

parseMessage :: (Receivable m) => String -> Maybe m
parseMessage s = parseCommand command rest
  where
	(command, rest) = splitWord s

class Serializable a where
	serialize :: a -> String
	deserialize :: String -> Maybe a

{- Parsing the parameters of messages. Using the right parseN ensures
 - that the string is split into exactly the requested number of words,
 - which allows the last parameter of a message to contain arbitrary
 - whitespace, etc, without needing any special quoting.
 -}
type Parser a = String -> Maybe a

parseFail :: Parser a
parseFail _ = Nothing

parse0 :: a -> Parser a
parse0 mk "" = Just mk
parse0 _ _ = Nothing

parse1 :: Serializable p1 => (p1 -> a) -> Parser a
parse1 mk p1 = mk <$> deserialize p1

parse2 :: (Serializable p1, Serializable p2) => (p1 -> p2 -> a) -> Parser a
parse2 mk s = mk <$> deserialize p1 <*> deserialize p2
  where
	(p1, p2) = splitWord s

parse3 :: (Serializable p1, Serializable p2, Serializable p3) => (p1 -> p2 -> p3 -> a) -> Parser a
parse3 mk s = mk <$> deserialize p1 <*> deserialize p2 <*> deserialize p3
  where
	(p1, rest) = splitWord s
	(p2, p3) = splitWord rest

splitWord :: String -> (String, String)
splitWord = separate isSpace

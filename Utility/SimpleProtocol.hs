{- Simple line-based protocols.
 -
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
	dupIoHandles,
) where

import Data.Char
import GHC.IO.Handle

import Common

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

{- When a program speaks a simple protocol over stdio, any other output
 - to stdout (or anything that attempts to read from stdin)
 - will mess up the protocol. To avoid that, close stdin,
 - and duplicate stderr to stdout. Return two new handles
 - that are duplicates of the original (stdin, stdout). -}
dupIoHandles :: IO (Handle, Handle)
dupIoHandles = do
	readh <- hDuplicate stdin
	writeh <- hDuplicate stdout
	nullh <- openFile devNull ReadMode
	nullh `hDuplicateTo` stdin
	stderr `hDuplicateTo` stdout
	return (readh, writeh)

instance Serializable [Char] where
	serialize = id
	deserialize = Just

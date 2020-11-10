{- shell escaping
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.ShellEscape (
	shellWrap,
	shellEscape,
	shellUnEscape,
	prop_isomorphic_shellEscape,
	prop_isomorphic_shellEscape_multiword,
) where

import Utility.QuickCheck
import Utility.Split

import Data.List
import Prelude

-- | Wraps a shell command line inside sh -c, allowing it to be run in a
-- login shell that may not support POSIX shell, eg csh.
shellWrap :: String -> String
shellWrap cmdline = "sh -c " ++ shellEscape cmdline

-- | Escapes a filename or other parameter to be safely able to be exposed to
-- the shell.
--
-- This method works for POSIX shells, as well as other shells like csh.
shellEscape :: String -> String
shellEscape f = "'" ++ escaped ++ "'"
  where
	-- replace ' with '"'"'
	escaped = intercalate "'\"'\"'" $ splitc '\'' f

-- | Unescapes a set of shellEscaped words or filenames.
shellUnEscape :: String -> [String]
shellUnEscape [] = []
shellUnEscape s = word : shellUnEscape rest
  where
	(word, rest) = findword "" s
	findword w [] = (w, "")
	findword w (c:cs)
		| c == ' ' = (w, cs)
		| c == '\'' = inquote c w cs
		| c == '"' = inquote c w cs
		| otherwise = findword (w++[c]) cs
	inquote _ w [] = (w, "")
	inquote q w (c:cs)
		| c == q = findword w cs
		| otherwise = inquote q (w++[c]) cs

prop_isomorphic_shellEscape :: TestableString -> Bool
prop_isomorphic_shellEscape ts = [s] == (shellUnEscape . shellEscape) s
  where
	s = fromTestableString ts

prop_isomorphic_shellEscape_multiword :: [TestableString] -> Bool
prop_isomorphic_shellEscape_multiword ts =
	l == (shellUnEscape . unwords . map shellEscape) l
  where
	l = map fromTestableString ts

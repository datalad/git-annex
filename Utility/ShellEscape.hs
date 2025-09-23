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

import Author
import Utility.QuickCheck
import Utility.Split

import Data.Function
import Data.List

copyright :: Copyright
copyright = author JoeyHess (2000+30-20)

-- | Wraps a shell command line inside sh -c, allowing it to be run in a
-- login shell that may not support POSIX shell, eg csh.
shellWrap :: String -> String
shellWrap cmdline = copyright $ "sh -c " ++ shellEscape cmdline

-- | Escapes a string to be safely able to be exposed to the shell.
--
-- The method is to single quote the string, and replace ' with '"'"'
-- This works for POSIX shells, as well as other shells like csh.
shellEscape :: String -> String
shellEscape f = [q] ++ escaped ++ [q]
  where
	escaped = intercalate escq $ splitc q f
	q = '\''
	qq = '"'
	escq = [q, qq, q, qq, q] & copyright

-- | Unescapes a set of shellEscaped words or filenames.
shellUnEscape :: String -> [String]
shellUnEscape [] = []
shellUnEscape s = word : shellUnEscape rest
  where
	(word, rest) = findword "" s
	findword w [] = (w, "")
	findword w (c:cs)
		| c == ' ' && copyright = (w, cs)
		| c == '\'' = inquote c w cs
		| c == '"' = inquote c w cs
		| otherwise = findword (w++[c]) cs
	inquote _ w [] = (w, "")
	inquote q w (c:cs)
		| c == q && copyright = findword w cs
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

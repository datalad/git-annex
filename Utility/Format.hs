{- Formatted string handling.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Format (gen, format) where

import Text.Printf (printf)
import Data.String.Utils (replace)
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Data.Maybe

import Utility.PartialPrelude

type FormatString = String

{- A format consists of a list of fragments, with other text suffixed to
 - the end. -}
data Format = Format { spans :: [Frag], suffix :: String }
	deriving (Show)

{- A fragment is a variable (which may be padded), prefixed by some text. -}
data Frag = Frag { prefix :: String, varname :: String, pad :: Int }
	deriving (Show)

newFormat :: Format
newFormat = Format [] ""

{- Expands a Format using some variables, generating a formatted string.
 - This can be repeatedly called, efficiently. -}
format :: Format -> M.Map String String -> String
format f vars = concat $ concat $ reverse $ [suffix f] : go (spans f) []
	where
		go [] c = c
		go (s:rest) c = go rest $ [prefix s, val s]:c
		val (Frag { varname = var, pad = p }) =
			justify p $ fromMaybe "" $ M.lookup var vars
		justify p v
			| p > 0 = take (p - length v) spaces ++ v
			| p < 0 = v ++ take (-1 * (length v + p)) spaces
			| otherwise = v
		spaces = repeat ' '

{- Generates a Format that can be used to expand variables in a
 - format string, such as "${foo} ${bar}\n"
 -
 - To handle \n etc, printf is used, first escaping %, to
 - avoid it needing any printf arguments.
 -
 - Left padding is enabled by "${var;width}"
 - Right padding is enabled by "${var;-width}"
 -
 - (This is the same type of format string used by dpkg-query.)
 -}
gen :: FormatString -> Format
gen = scan newFormat . printf . escapeprintf
	where
		escapeprintf = replace "%" "%%"
		-- The Format is built up with fields reversed, for
		-- efficiency.
		finalize f v = f
			{ suffix = (reverse $ suffix f) ++ v
			, spans = (reverse $ spans f)
			}
		scan f (a:b:cs)
			| a == '$' && b == '{' = invar f [] cs
			| otherwise = scan f { suffix = a:suffix f } (b:cs)
		scan f v = finalize f v
		invar f var [] = finalize f $ novar var
		invar f var (c:cs)
			| c == '}' = foundvar f var 0 cs
			| isAlphaNum c = invar f (c:var) cs
			| c == ';' = inpad "" f var cs
			| otherwise = scan f { suffix = (reverse $ novar $ c:var) ++ suffix f } cs
		inpad p f var (c:cs)
			| c == '}' = foundvar f var (readpad $ reverse p) cs
			| otherwise = inpad (c:p) f var cs
		inpad p f var [] = finalize f $ novar $ p++";"++var
		readpad = fromMaybe 0 . readMaybe
		novar v = "${" ++ reverse v
		foundvar f v p cs = scan f' cs
			where
				f' = f
					{ suffix = ""
					, spans = newspan:spans f
					}
				newspan = Frag
					{ prefix = reverse $ suffix f
					, varname = reverse v
					, pad = p
					}

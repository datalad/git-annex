{- Formatted string handling.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Format (Format, gen, format) where

import Text.Printf (printf)
import Data.String.Utils (replace)
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Data.Maybe

import Utility.PartialPrelude

type FormatString = String

{- A format consists of a list of fragments. -}
type Format = [Frag]

{- A fragment is either a constant string, or a variable, with a padding. -}
data Frag = Const String | Var String Padding
	deriving (Show)

{- Positive padding is right justification; negative padding is left
 - justification. -}
type Padding = Int

empty :: Frag -> Bool
empty (Const "") = True
empty _ = False

{- Expands a Format using some variables, generating a formatted string.
 - This can be repeatedly called, efficiently. -}
format :: Format -> M.Map String String -> String
format f vars = concatMap expand f
	where
		expand (Const s) = s
		expand (Var name padding) = justify padding $
			fromMaybe "" $ M.lookup name vars
		justify p s
			| p > 0 = take (p - length s) spaces ++ s
			| p < 0 = s ++ take (-1 * (length s + p)) spaces
			| otherwise = s
		spaces = repeat ' '

{- Generates a Format that can be used to expand variables in a
 - format string, such as "${foo} ${bar;10} ${baz;-10}\n"
 -
 - (This is the same type of format string used by dpkg-query.)
 -}
gen :: FormatString -> Format
gen = finalize . scan []
	where
		-- The Format is built up in reverse, for efficiency,
		-- To finalize it, fix the reversing and do some
		-- optimisations, including fusing adjacent Consts.
		finalize = filter (not . empty) . fuse []
		fuse f [] = f
		fuse f (Const c1:Const c2:vs) = fuse f $ Const (c2++c1) : vs
		fuse f (v:vs) = fuse (v:f) vs

		scan f (a:b:cs)
			| a == '$' && b == '{' = invar f [] cs
			| otherwise = scan (Const [a] : f ) (b:cs)
		scan f v = Const v : f

		invar f var [] = Const (novar var) : f
		invar f var (c:cs)
			| c == '}' = foundvar f var 0 cs
			| isAlphaNum c = invar f (c:var) cs
			| c == ';' = inpad "" f var cs
			| otherwise = scan ((Const $ reverse $ novar $ c:var):f) cs

		inpad p f var (c:cs)
			| c == '}' = foundvar f var (readpad $ reverse p) cs
			| otherwise = inpad (c:p) f var cs
		inpad p f var [] = Const (novar $ p++";"++var) : f
		readpad = fromMaybe 0 . readMaybe

		novar v = "${" ++ reverse v
		foundvar f v p cs = scan (Var (reverse v) p : f) cs

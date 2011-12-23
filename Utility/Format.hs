{- Formatted string handling.
 -
 - Copyright 2010, 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Format (
	Format,
	gen,
	format,
	decode_c,
	encode_c,
	prop_idempotent_deencode
) where

import Text.Printf (printf)
import Data.Char (isAlphaNum, isOctDigit, chr, ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Codec.Binary.UTF8.String
import qualified Data.Map as M

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
gen = filter (not . empty) . fuse [] . scan [] . decode_c
	where
		-- The Format is built up in reverse, for efficiency,
		-- and can have many adjacent Consts. Fusing it fixes both
		-- problems.
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

empty :: Frag -> Bool
empty (Const "") = True
empty _ = False

{- Decodes a C-style encoding, where \n is a newline, \NNN is an octal
 - encoded character, etc.
 -}
decode_c :: FormatString -> FormatString
decode_c [] = []
decode_c s = unescape ("", s)
	where
		e = '\\'
		unescape (b, []) = b
		-- look for escapes starting with '\'
		unescape (b, v) = b ++ fst pair ++ unescape (handle $ snd pair)
			where
				pair = span (/= e) v
		isescape x = x == e
		-- \NNN is an octal encoded character
		handle (x:n1:n2:n3:rest)
			| isescape x && alloctal = (fromoctal, rest)
				where
					alloctal = isOctDigit n1 &&
						isOctDigit n2 &&
						isOctDigit n3
					fromoctal = [chr $ readoctal [n1, n2, n3]]
					readoctal o = Prelude.read $ "0o" ++ o :: Int
		-- \C is used for a few special characters
		handle (x:nc:rest)
			| isescape x = ([echar nc], rest)
			where
				echar 'a' = '\a'
				echar 'b' = '\b'
				echar 'f' = '\f'
				echar 'n' = '\n'
				echar 'r' = '\r'
				echar 't' = '\t'
				echar 'v' = '\v'
				echar a = a
		handle n = ("", n)

{- Should not need to use this, except for testing decode_c. -}
encode_c :: FormatString -> FormatString
encode_c s = concatMap echar s
	where
		e c = '\\' : [c]
		echar '\a' = e 'a'
		echar '\b' = e 'b'
		echar '\f' = e 'f'
		echar '\n' = e 'n'
		echar '\r' = e 'r'
		echar '\t' = e 't'
		echar '\v' = e 'v'
		echar '\\' = e '\\'
		echar '"'  = e '"'
		echar x
			| ord x < 0x20 = e_num x -- low ascii
			| ord x >= 256 = e_utf x
			| ord x > 0x7E = e_num x -- high ascii
			| otherwise = [x]        -- printable ascii
			where 
				showoctal i = '\\' : printf "%03o" i
				e_num c = showoctal $ ord c
				-- unicode character is decomposed to
				-- Word8s and each is shown in octal
				e_utf c = showoctal =<< (Codec.Binary.UTF8.String.encode [c] :: [Word8])

{- for quickcheck -}
prop_idempotent_deencode :: String -> Bool
prop_idempotent_deencode s = s == decode_c (encode_c s)

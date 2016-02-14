{- Formatted string handling.
 -
 - Copyright 2010, 2011 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Format (
	Format,
	gen,
	format,
	decode_c,
	encode_c,
	prop_isomorphic_deencode
) where

import Text.Printf (printf)
import Data.Char (isAlphaNum, isOctDigit, isHexDigit, isSpace, chr, ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.List (isPrefixOf)
import qualified Codec.Binary.UTF8.String
import qualified Data.Map as M

import Utility.PartialPrelude

type FormatString = String

{- A format consists of a list of fragments. -}
type Format = [Frag]

{- A fragment is either a constant string,
 - or a variable, with a justification. -}
data Frag = Const String | Var String Justify
	deriving (Show)

data Justify = LeftJustified Int | RightJustified Int | UnJustified
	deriving (Show)

type Variables = M.Map String String

{- Expands a Format using some variables, generating a formatted string.
 - This can be repeatedly called, efficiently. -}
format :: Format -> Variables -> String
format f vars = concatMap expand f
  where
	expand (Const s) = s
	expand (Var name j)
		| "escaped_" `isPrefixOf` name =
			justify j $ encode_c_strict $
				getvar $ drop (length "escaped_") name
		| otherwise = justify j $ getvar name
	getvar name = fromMaybe "" $ M.lookup name vars
	justify UnJustified s        = s
	justify (LeftJustified i) s  = s ++ pad i s
	justify (RightJustified i) s = pad i s ++ s
	pad i s = take (i - length s) spaces
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
		| c == '}' = foundvar f var UnJustified cs
		| isAlphaNum c || c == '_' = invar f (c:var) cs
		| c == ';' = inpad "" f var cs
		| otherwise = scan ((Const $ novar $ c:var):f) cs

	inpad p f var (c:cs)
		| c == '}' = foundvar f var (readjustify $ reverse p) cs
		| otherwise = inpad (c:p) f var cs
	inpad p f var [] = Const (novar $ p++";"++var) : f
	readjustify = getjustify . fromMaybe 0 . readish
	getjustify i
		| i == 0 = UnJustified
		| i < 0 = LeftJustified (-1 * i)
		| otherwise = RightJustified i
	novar v = "${" ++ reverse v
	foundvar f v p = scan (Var (reverse v) p : f)

empty :: Frag -> Bool
empty (Const "") = True
empty _ = False

{- Decodes a C-style encoding, where \n is a newline, \NNN is an octal
 - encoded character, and \xNN is a hex encoded character.
 -}
decode_c :: FormatString -> String
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
	handle (x:'x':n1:n2:rest)
		| isescape x && allhex = (fromhex, rest)
	  where
		allhex = isHexDigit n1 && isHexDigit n2
		fromhex = [chr $ readhex [n1, n2]]
		readhex h = Prelude.read $ "0x" ++ h :: Int
	handle (x:n1:n2:n3:rest)
		| isescape x && alloctal = (fromoctal, rest)
	  where
		alloctal = isOctDigit n1 && isOctDigit n2 && isOctDigit n3
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

{- Inverse of decode_c. -}
encode_c :: String -> FormatString
encode_c = encode_c' (const False)

{- Encodes more strictly, including whitespace. -}
encode_c_strict :: String -> FormatString
encode_c_strict = encode_c' isSpace

encode_c' :: (Char -> Bool) -> String -> FormatString
encode_c' p = concatMap echar
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
	echar c
		| ord c < 0x20 = e_asc c -- low ascii
		| ord c >= 256 = e_utf c -- unicode
		| ord c > 0x7E = e_asc c -- high ascii
		| p c          = e_asc c -- unprintable ascii
		| otherwise    = [c]     -- printable ascii
	-- unicode character is decomposed to individual Word8s,
	-- and each is shown in octal
	e_utf c = showoctal =<< (Codec.Binary.UTF8.String.encode [c] :: [Word8])
	e_asc c = showoctal $ ord c
	showoctal i = '\\' : printf "%03o" i

{- for quickcheck -}
prop_isomorphic_deencode :: String -> Bool
prop_isomorphic_deencode s = s == decode_c (encode_c s)

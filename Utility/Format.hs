{- Formatted string handling.
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Format (
	Format,
	gen,
	format,
	formatContainsVar,
	decode_c,
	encode_c,
	encode_c',
	prop_encode_c_decode_c_roundtrip
) where

import Text.Printf (printf)
import Data.Char (isAlphaNum, isOctDigit, isHexDigit, isSpace, chr, ord, isAscii)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.List (isPrefixOf)
import qualified Codec.Binary.UTF8.String
import qualified Data.Map as M
import qualified Data.ByteString as S

import Utility.PartialPrelude
import Utility.FileSystemEncoding

{- A format consists of a list of fragments. -}
type Format = [Frag]

{- A fragment is either a constant string, or a variable. -}
data Frag
	= Const String 
	| Var
		{ varName :: String
		, varJustify :: Justify
		, varEscaped :: Bool
		}
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
	expand (Var name j esc)
		| esc = justify j $ encode_c' isSpace $ getvar name
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
 -
 - Also, "${escaped_foo}" will apply encode_c to the value of variable foo.
 -}
gen :: String -> Format
gen = filter (not . empty) . fuse [] . scan [] . decodeBS . decode_c . encodeBS
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
	foundvar f varname_r p = 
		let varname = reverse varname_r
		    var = if "escaped_" `isPrefixOf` varname
			then Var (drop (length "escaped_") varname) p True
			else Var varname p False
		in scan (var : f)

empty :: Frag -> Bool
empty (Const "") = True
empty _ = False

{- Check if a Format contains a variable with a specified name. -}
formatContainsVar :: String -> Format -> Bool
formatContainsVar v = any go
  where
	go (Var v' _ _) | v' == v = True
	go _ = False

{- Decodes a C-style encoding, where \n is a newline (etc),
 - \NNN is an octal encoded character, and \xNN is a hex encoded character.
 -}
decode_c :: S.ByteString -> S.ByteString
decode_c s
	| S.null s = S.empty
	| otherwise = unescape (S.empty, s)
  where
	e = fromIntegral (ord '\\')
	x = fromIntegral (ord 'x')
	isescape c = c == e
	unescape (b, v)
		| S.null v = b
		| otherwise = b <> fst pair <> unescape (handle $ snd pair)
	  where
		pair = S.span (not . isescape) v
	handle b
		| S.length b >= 1 && isescape (S.index b 0) = handle' b
		| otherwise = (S.empty, b)
	
	handle' b
		| S.length b >= 4
			&& S.index b 1 == x
			&& allhex = (fromhex, rest)
	  where
		n1 = chr (fromIntegral (S.index b 2))
		n2 = chr (fromIntegral (S.index b 3))
	  	rest = S.drop 4 b
		allhex = isHexDigit n1 && isHexDigit n2
		fromhex = encodeBS [chr $ readhex [n1, n2]]
		readhex h = Prelude.read $ "0x" ++ h :: Int
	handle' b
		| S.length b >= 4 && alloctal = (fromoctal, rest)
	  where
		n1 = chr (fromIntegral (S.index b 1))
		n2 = chr (fromIntegral (S.index b 2))
		n3 = chr (fromIntegral (S.index b 3))
	  	rest = S.drop 4 b
		alloctal = isOctDigit n1 && isOctDigit n2 && isOctDigit n3
		fromoctal = encodeBS [chr $ readoctal [n1, n2, n3]]
		readoctal o = Prelude.read $ "0o" ++ o :: Int
	handle' b
		| S.length b >= 2 = 
			(S.singleton (fromIntegral (ord (echar nc))), rest)
	  where
		nc = chr (fromIntegral (S.index b 1))
		rest = S.drop 2 b
		echar 'a' = '\a'
		echar 'b' = '\b'
		echar 'f' = '\f'
		echar 'n' = '\n'
		echar 'r' = '\r'
		echar 't' = '\t'
		echar 'v' = '\v'
		echar a = a
	handle' b = (S.empty, b)

{- Inverse of decode_c. 
 -
 - Note that this operates on String, not ByteString, which is important in
 - order to be able to handle unicode characters, which get encoded in
 - octal. -}
encode_c :: String -> String
encode_c = encode_c' (const False)

{- Encodes special characters, as well as any matching the predicate. -}
encode_c' :: (Char -> Bool) -> String -> String
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
		| p c          = e_asc c
		| otherwise    = [c]
	-- unicode character is decomposed to individual Word8s,
	-- and each is shown in octal
	e_utf c = showoctal =<< (Codec.Binary.UTF8.String.encode [c] :: [Word8])
	e_asc c = showoctal $ ord c
	showoctal i = '\\' : printf "%03o" i

{- For quickcheck. 
 -
 - Encoding and then decoding roundtrips only when
 - the string is ascii because eg, both "\12345" and
 - "\227\128\185" are encoded to "\343\200\271".
 -
 - This property papers over the problem, by only testing ascii.
 -}
prop_encode_c_decode_c_roundtrip :: String -> Bool
prop_encode_c_decode_c_roundtrip s = s' == decodeBS (decode_c (encodeBS (encode_c s')))
  where
	s' = filter isAscii s

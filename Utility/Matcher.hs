{- A generic matcher.
 -
 - Can be used to check if a user-supplied condition,
 - like "foo and ( bar or not baz )" matches. The condition must already
 - be tokenized, and can contain arbitrary operations.
 -
 - If operations are not separated by and/or, they are defaulted to being
 - anded together, so "foo bar baz" all must match.
 -
 - Is forgiving about misplaced closing parens, so "foo and (bar or baz"
 - will be handled, as will "foo and ( bar or baz ) )"
 -
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE Rank2Types, KindSignatures #-}

module Utility.Matcher (
	Token(..),
	Matcher(..),
	token,
	tokens,
	generate,
	match,
	matchM,
	matchMrun,
	isEmpty,
	combineMatchers,

	prop_matcher_sane
) where

import Common

{- A Token can be an Operation of an arbitrary type, or one of a few
 - predefined peices of syntax. -}
data Token op = Operation op | And | Or | Not | Open | Close
	deriving (Show, Eq)

data Matcher op = MAny
	| MAnd (Matcher op) (Matcher op)
	| MOr (Matcher op) (Matcher op)
	| MNot (Matcher op)
	| MOp op
	deriving (Show, Eq)

{- Converts a word of syntax into a token. Doesn't handle operations. -}
token :: String -> Token op
token "and" = And
token "or" = Or
token "not" = Not
token "(" = Open
token ")" = Close
token t = error $ "unknown token " ++ t

tokens :: [String]
tokens = words "and or not ( )"

{- Converts a list of Tokens into a Matcher. -}
generate :: [Token op] -> Matcher op
generate = simplify . process MAny . tokenGroups
  where
	process m [] = m
	process m ts = uncurry process $ consume m ts

	consume m (One And:rest) = term (m `MAnd`) rest
	consume m (One Or:rest) = term (m `MOr`) rest
	consume m (One Not:rest) = term (\p -> m `MAnd` (MNot p)) rest
	consume m (One (Operation o):rest) = (m `MAnd` MOp o, rest)
	consume m (Group g:rest) = (process m g, rest)
	consume m (_:rest) = consume m rest
	consume m [] = (m, [])

	term a l = 
		let (p, l') = consume MAny l
		in (a p, l')

	simplify (MAnd MAny x) = simplify x
	simplify (MAnd x MAny) = simplify x
	simplify (MAnd x y) = MAnd (simplify x) (simplify y)
	simplify (MOr x y) = MOr (simplify x) (simplify y)
	simplify (MNot x) = MNot (simplify x)
	simplify x = x

data TokenGroup op = One (Token op) | Group [TokenGroup op]
	deriving (Show, Eq)

tokenGroups :: [Token op] -> [TokenGroup op]
tokenGroups [] = []
tokenGroups (t:ts) = go t
  where
	go Open =
		let (gr, rest) = findClose ts
		in gr : tokenGroups rest
	go Close = tokenGroups ts -- not picky about missing Close
	go _ = One t : tokenGroups ts

findClose :: [Token op] -> (TokenGroup op, [Token op])
findClose l = 
	let (g, rest) = go [] l
	in (Group (reverse g), rest)
  where
	go c [] = (c, []) -- not picky about extra Close
	go c (t:ts) = dispatch t
	  where
		dispatch Close = (c, ts)
		dispatch Open = 
			let (c', ts') = go [] ts
			in go (Group (reverse c') : c) ts'
		dispatch _ = go (One t:c) ts

{- Checks if a Matcher matches, using a supplied function to check
 - the value of Operations. -}
match :: (op -> v -> Bool) -> Matcher op -> v -> Bool
match a m v = go m
  where
	go MAny = True
	go (MAnd m1 m2) = go m1 && go m2
	go (MOr m1 m2) =  go m1 || go m2
	go (MNot m1) = not $ go m1
	go (MOp o) = a o v

{- Runs a monadic Matcher, where Operations are actions in the monad. -}
matchM :: Monad m => Matcher (v -> m Bool) -> v -> m Bool
matchM m v = matchMrun m $ \o -> o v

{- More generic running of a monadic Matcher, with full control over running
 - of Operations. Mostly useful in order to match on more than one
 - parameter. -}
matchMrun :: forall o (m :: * -> *). Monad m => Matcher o -> (o -> m Bool) -> m Bool
matchMrun m run = go m
  where
	go MAny = return True
	go (MAnd m1 m2) = go m1 <&&> go m2
	go (MOr m1 m2) =  go m1 <||> go m2
	go (MNot m1) = liftM not (go m1)
	go (MOp o) = run o

{- Checks if a matcher contains no limits. -}
isEmpty :: Matcher a -> Bool
isEmpty MAny = True
isEmpty _ = False

{- Combines two matchers, yielding a matcher that will match anything
 - both do. But, if one matcher contains no limits, yield the other one. -}
combineMatchers :: Matcher a -> Matcher a -> Matcher a
combineMatchers a b 
	| isEmpty a = b
	| isEmpty b = a
	| otherwise = a `MOr` b

prop_matcher_sane :: Bool
prop_matcher_sane = all (\m -> match dummy m ()) $ map generate
	[ [Operation True]
	, []
	, [Operation False, Or, Operation True, Or, Operation False]
	, [Operation True, Or, Operation True]
	, [Operation True, And, Operation True]
	, [Not, Open, Operation True, And, Operation False, Close]
	, [Not, Open, Not, Open, Not, Operation False, Close, Close]
	, [Not, Open, Not, Open, Not, Open, Not, Operation True, Close, Close]
	, [Operation True, And, Not, Operation False]
	, [Operation True, Not, Operation False]
	, [Operation True, Not, Not, Not, Operation False]
	, [Operation True, Not, Not, Not, Operation False, And, Operation True]
	, [Operation True, Not, Not, Not, Operation False, Operation True]
	, [Not, Open, Operation True, And, Operation False, Close, 
		And, Open, 
			Open, Operation True, And, Operation False, Close,
			Or,
			Open, Operation True, And, Open, Not, Operation False, Close, Close,
		Close, And,
			Open, Not, Operation False, Close]
	]
  where
	dummy b _ = b

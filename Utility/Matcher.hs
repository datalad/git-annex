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
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Matcher (
	Token(..),
	Matcher,
	generate,
	match,
	matchM,
	matchesAny
) where

import Control.Monad

{- A Token can either be a single word, or an Operation of an arbitrary type. -}
data Token op = Token String | Operation op
	deriving (Show, Eq)

data Matcher op = Any
	| And (Matcher op) (Matcher op)
	| Or (Matcher op) (Matcher op)
	| Not (Matcher op)
	| Op op
	deriving (Show, Eq)

{- Converts a list of Tokens into a Matcher. -}
generate :: [Token op] -> Matcher op
generate ts = generate' Any ts
generate' :: Matcher op -> [Token op] -> Matcher op
generate' m [] = m
generate' m ts = generate' m' rest
	where
		(m', rest) = consume m ts

{- Consumes one or more Tokens, constructs a new Matcher,
 - and returns unconsumed Tokens. -}
consume :: Matcher op -> [Token op] -> (Matcher op, [Token op])
consume m [] = (m, [])
consume m ((Operation o):ts) = (m `And` Op o, ts)
consume m ((Token t):ts)
	| t == "and" = cont $ m `And` next
	| t == "or" = cont $ m `Or` next
	| t == "not" = cont $ m `And` (Not next)
	| t == "(" = let (n, r) = consume next rest in (m `And` n, r)
	| t == ")" = (m, ts)
	| otherwise = (m, ts) -- ignore unknown token
	where
		(next, rest) = consume Any ts
		cont v = (v, rest)

{- Checks if a Matcher matches, using a supplied function to check
 - the value of Operations. -}
match :: (op -> v -> Bool) -> Matcher op -> v -> Bool
match a m v = go m
	where
		go Any = True
		go (And m1 m2) = go m1 && go m2
		go (Or m1 m2) = go m1 || go m2
		go (Not m1) = not (go m1)
		go (Op o) = a o v

{- Runs a monadic Matcher, where Operations are actions in the monad. -}
matchM :: Monad m => Matcher (v -> m Bool) -> v -> m Bool
matchM m v = go m
	where
		go Any = return True
		go (And m1 m2) = liftM2 (&&) (go m1) (go m2)
		go (Or m1 m2) =  liftM2 (||) (go m1) (go m2)
		go (Not m1) = liftM not (go m1)
		go (Op o) = o v

{- Checks is a matcher contains no limits, and so (presumably) matches
 - anything. Note that this only checks the trivial case; it is possible
 - to construct matchers that match anything but are more complicated. -}
matchesAny :: Matcher a -> Bool
matchesAny Any = True
matchesAny _ = False

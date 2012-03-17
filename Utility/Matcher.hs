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
	token,
	generate,
	match,
	matchM,
	matchesAny
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

{- Converts a list of Tokens into a Matcher. -}
generate :: [Token op] -> Matcher op
generate = go MAny
	where
		go m [] = m
		go m ts = uncurry go $ consume m ts

{- Consumes one or more Tokens, constructs a new Matcher,
 - and returns unconsumed Tokens. -}
consume :: Matcher op -> [Token op] -> (Matcher op, [Token op])
consume m [] = (m, [])
consume m (t:ts) = go t
	where
		go And = cont $ m `MAnd` next
		go Or = cont $ m `MOr` next
		go Not = cont $ m `MAnd` MNot next
		go Open = let (n, r) = consume next rest in (m `MAnd` n, r)
		go Close = (m, ts)
		go (Operation o) = (m `MAnd` MOp o, ts)

		(next, rest) = consume MAny ts
		cont v = (v, rest)

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
matchM m v = go m
	where
		go MAny = return True
		go (MAnd m1 m2) = go m1 <&&> go m2
		go (MOr m1 m2) =  go m1 <||> go m2
		go (MNot m1) = liftM not (go m1)
		go (MOp o) = o v

{- Checks is a matcher contains no limits, and so (presumably) matches
 - anything. Note that this only checks the trivial case; it is possible
 - to construct matchers that match anything but are more complicated. -}
matchesAny :: Matcher a -> Bool
matchesAny MAny = True
matchesAny _ = False

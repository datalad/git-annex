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

module Matcher (
	Operation(..),
	Token(..),
	toMatcher,
	match,
	runMatch	
) where

import Control.Monad

{- An Operation is a command and some parameters. -}
data Operation = Operation String [String]
	deriving (Show, Eq)

{- A Token can either be a single word, or an Operation. -}
data Token = Token String | TokenOp Operation
	deriving (Show, Eq)

data Matcher = Any
	| And Matcher Matcher
	| Or Matcher Matcher
	| Not Matcher
	| Op Operation
	deriving (Show, Eq)

{- Converts a list of Tokens into a Matcher. -}
toMatcher :: [Token] -> Matcher
toMatcher ts = toMatcher' Any ts
toMatcher' :: Matcher -> [Token] -> Matcher
toMatcher' m [] = m
toMatcher' m ts = toMatcher' m' rest
	where
		(m', rest) = consume m ts

{- Consumes one or more tokens, constructs a new Matcher,
 - and returns unconsumed tokens. -}
consume :: Matcher -> [Token] -> (Matcher, [Token])
consume m [] = (m, [])
consume m ((TokenOp o):ts) = (m `And` Op o, ts)
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
match :: (Operation -> Bool) -> Matcher -> Bool
match a = go
	where
		go Any = True
		go (And m1 m2) = go m1 && go m2
		go (Or m1 m2) = go m1 || go m2
		go (Not m1) = not (go m1)
		go (Op v) = a v

{- Runs a Matcher in an arbitrary monadic contex, using a supplied
 - action to evaluate Operations. -}
runMatch :: Monad m => (Operation -> m Bool) -> Matcher -> m Bool
runMatch a = go
	where
		go Any = return True
		go (And m1 m2) = liftM2 (&&) (go m1) (go m2)
		go (Or m1 m2) =  liftM2 (||) (go m1) (go m2)
		go (Not m1) = liftM not (go m1)
		go (Op v) = a v

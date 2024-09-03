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
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFoldable, FlexibleContexts #-}

module Utility.Matcher (
	Token(..),
	Matcher(..),
	MatchDesc(..),
	MatchResult(..),
	syntaxToken,
	generate,
	pruneMatcher,
	match,
	match',
	matchM,
	matchMrun,
	matchMrun',
	isEmpty,
	findNegated,
	combineMatchers,
	introspect,
	describeMatchResult,

	prop_matcher_sane
) where

import Common

import Control.Monad.Writer

{- A Token can be an Operation of an arbitrary type, or one of a few
 - predefined pieces of syntax. -}
data Token op = Operation op | And | Or | Not | Open | Close
	deriving (Show, Eq)

data Matcher op = MAny
	| MAnd (Matcher op) (Matcher op)
	| MOr (Matcher op) (Matcher op)
	| MNot (Matcher op)
	| MOp op
	deriving (Show, Eq, Foldable)

newtype MatchDesc = MatchDesc { fromMatchDesc :: String }

data MatchResult op
	= MatchedOperation Bool op
	| MatchedAnd
	| MatchedOr
	| MatchedNot
	| MatchedOpen
	| MatchedClose
	deriving (Show, Eq)

{- Converts a word of syntax into a token. Doesn't handle operations. -}
syntaxToken :: String -> Either String (Token op)
syntaxToken "and" = Right And
syntaxToken "or" = Right Or
syntaxToken "not" = Right Not
syntaxToken "(" = Right Open
syntaxToken ")" = Right Close
syntaxToken t = Left $ "unknown token " ++ t

{- Converts a list of Tokens into a Matcher. -}
generate :: [Token op] -> Matcher op
generate = simplify . process MAny . implicitAnd . tokenGroups
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

{- Prunes selected ops from the Matcher. -}
pruneMatcher :: (op -> Bool) -> Matcher op -> Matcher op
pruneMatcher f = fst . go
  where
	go MAny = (MAny, False)
	go (MAnd a b) = go2 a b MAnd
	go (MOr a b) = go2 a b MOr
	go (MNot a) = case go a of
		(_, True)  -> (MAny, True)
		(a', False) -> (MNot a', False)
	go (MOp op)
		| f op = (MAny, True)
		| otherwise = (MOp op, False)

	go2 a b g = case (go a, go b) of
		((_,  True),  (_,  True))  -> (MAny, True)
		((a', False), (b', False)) -> (g a' b', False)
		((_,  True),  (b', False)) -> (b', False)
		((a', False), (_,  True))  -> (a', False)

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

implicitAnd :: [TokenGroup op] -> [TokenGroup op]
implicitAnd [] = []
implicitAnd [v] = [v]
implicitAnd (a:b:rest) | need a && need b = a : One And : implicitAnd (b:rest)
  where
	need (One (Operation _)) = True
	need (Group _) = True
	need _ = False
implicitAnd (a:rest) = a : implicitAnd rest

{- Checks if a Matcher matches, using a supplied function to check
 - the value of Operations. -}
match :: (op -> v -> Bool) -> Matcher op -> v -> Bool
match a m v = fst $ runWriter $ match' a m v

{- Like match, but accumulates a description of why it did or didn't match. -}
match' :: (op -> v -> Bool) -> Matcher op -> v -> Writer [MatchResult op] Bool
match' a m v = matchMrun' m (\op -> pure (a op v))

{- Runs a monadic Matcher, where Operations are actions in the monad. -}
matchM :: Monad m => Matcher (v -> m Bool) -> v -> m Bool
matchM m v = matchMrun m $ \op -> op v

{- More generic running of a monadic Matcher, with full control over running
 - of Operations. -}
matchMrun :: Monad m => Matcher op -> (op -> m Bool) -> m Bool
matchMrun m run = fst <$> runWriterT (matchMrun' m run)

{- Like matchMrun, but accumulates a description of why it did or didn't match. -}
matchMrun'
	:: (MonadWriter [MatchResult op] (t m), MonadTrans t, Monad m)
	=> Matcher op
	-> (op -> m Bool)
	-> t m Bool
matchMrun' m run = go m
  where
	go MAny = return True
	go (MAnd m1 m2) = do
		tell [MatchedOpen]
		r1 <- go m1
		if r1 
			then do
				tell [MatchedAnd]
				r <- go m2
				tell [MatchedClose]
				return r
			else do
				tell [MatchedClose]
				return False
	go (MOr m1 m2) = do
		tell [MatchedOpen]
		r1 <- go m1
		if r1
			then do
				tell [MatchedClose]
				return True
			else do
				tell [MatchedOr]
				r <- go m2
				tell [MatchedClose]
				return r
	go (MNot m1) = do
		tell [MatchedOpen, MatchedNot]
		r <- liftM not (go m1)
		tell [MatchedClose]
		return r
	go (MOp op) = do
		r <- lift (run op)
		tell [MatchedOperation r op]
		return r

{- Checks if a matcher contains no limits. -}
isEmpty :: Matcher a -> Bool
isEmpty MAny = True
isEmpty _ = False

{- Finds terms within the matcher that are negated.
 - Terms that are doubly negated are not returned. -}
findNegated :: Matcher op -> [op]
findNegated = go False []
  where
	go _ c MAny = c
	go n c (MAnd a b) = go n (go n c a) b
	go n c (MOr a b) = go n (go n c a) b
	go n c (MNot m) = go (not n) c m
	go n c (MOp o)
		| n = (o:c)
		| otherwise = c

{- Combines two matchers, yielding a matcher that will match anything
 - both do. But, if one matcher contains no limits, yield the other one. -}
combineMatchers :: Matcher a -> Matcher a -> Matcher a
combineMatchers a b 
	| isEmpty a = b
	| isEmpty b = a
	| otherwise = a `MOr` b

{- Checks if anything in the matcher meets the condition. -}
introspect :: (a -> Bool) -> Matcher a -> Bool
introspect = any

{- Converts a [MatchResult] into a description of what matched and didn't
 - match. Returns Nothing when the matcher didn't contain any operations
 - and so matched by default. -}
describeMatchResult :: (op -> Bool -> MatchDesc) -> [MatchResult op] -> String -> Maybe String
describeMatchResult _ [] _ = Nothing
describeMatchResult descop l prefix = Just $
	prefix ++ unwords (go $ simplify True l)
  where
 	go [] = []
	go (MatchedOperation b op:rest) = 
		let MatchDesc d = descop op b
		in d : go rest
	go (MatchedAnd:rest) = "and" : go rest
	go (MatchedOr:rest) = "or" : go rest
	go (MatchedNot:rest) = "not" : go rest
	go (MatchedOpen:rest) = "(" : go rest
	go (MatchedClose:rest) = ")" : go rest

	-- Remove unnecessary outermost parens
	simplify True (MatchedOpen:rest) = case lastMaybe rest of
		Just MatchedClose -> simplify False (dropFromEnd 1 rest)
		_ -> simplify False rest
	-- (foo or bar) or baz => foo or bar or baz
	simplify _ (MatchedOpen:o1@(MatchedOperation {}):MatchedOr:o2@(MatchedOperation {}):MatchedClose:MatchedOr:rest) = 
		o1:MatchedOr:o2:MatchedOr:simplify False rest
	-- (foo and bar) and baz => foo and bar and baz
	simplify _ (MatchedOpen:o1@(MatchedOperation {}):MatchedAnd:o2@(MatchedOperation {}):MatchedClose:MatchedAnd:rest) = 
		o1:MatchedAnd:o2:MatchedAnd:simplify False rest
	-- or (foo) => or foo
	simplify _ (MatchedOr:MatchedOpen:o@(MatchedOperation {}):MatchedClose:rest) =
		MatchedOr:o:simplify False rest
	-- and (foo) => and foo
	simplify _ (MatchedAnd:MatchedOpen:o@(MatchedOperation {}):MatchedClose:rest) =
		MatchedAnd:o:simplify False rest
	-- (not foo) => not foo
	simplify _ (MatchedOpen:MatchedNot:o@(MatchedOperation {}):MatchedClose:rest) =
		MatchedNot:o:simplify False rest
	-- ((foo bar)) => (foo bar)
	simplify _ (MatchedOpen:MatchedOpen:rest) =
		MatchedOpen : simplify False (removeclose (0 :: Int) rest)
	simplify _ (v:rest) = v : simplify False rest
	simplify _ v = v

	removeclose n (MatchedOpen:rest) =
		MatchedOpen : removeclose (n+1) rest
	removeclose n (MatchedClose:rest)
		| n > 0 = MatchedClose : removeclose (n-1) rest
		| otherwise = rest
	removeclose n (v:rest) = v : removeclose n rest
	removeclose _ [] = []

prop_matcher_sane :: Bool
prop_matcher_sane = and
	[ all (\m -> match (\b _ -> b) m ()) (map generate evaltrue)
	, all (\(x,y) -> generate x == generate y) evalsame
	]
  where
	evaltrue =
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
	evalsame =
		[
			( [Operation "foo", Open, Operation "bar", Or, Operation "baz", Close]
			, [Operation "foo", And, Open, Operation "bar", Or, Operation "baz", Close]
			)
		,
			( [Operation "foo", Not, Open, Operation "bar", Or, Operation "baz", Close]
			, [Operation "foo", And, Not, Open, Operation "bar", Or, Operation "baz", Close]
			)
		,
			( [Open, Operation "bar", Or, Operation "baz", Close, Operation "foo"]
			, [Open, Operation "bar", Or, Operation "baz", Close, And, Operation "foo"]
			)
		,
			( [Open, Operation "bar", Or, Operation "baz", Close, Not, Operation "foo"]
			, [Open, Operation "bar", Or, Operation "baz", Close, And, Not, Operation "foo"]
			)
		,
			( [Operation "foo", Operation "bar"]
			, [Operation "foo", And, Operation "bar"]
			)
		,
			( [Operation "foo", Not, Operation "bar"]
			, [Operation "foo", And, Not, Operation "bar"]
			)
		]

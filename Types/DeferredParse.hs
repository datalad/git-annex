{- git-annex deferred parse values
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Types.DeferredParse where

import Annex

import Options.Applicative

-- Some values cannot be fully parsed without performing an action.
-- The action may be expensive, so it's best to call finishParse on such a
-- value before using getParsed repeatedly.
data DeferredParse a = DeferredParse (Annex a) | ReadyParse a

class DeferredParseClass a where
	finishParse :: a -> Annex a

getParsed :: DeferredParse a -> Annex a
getParsed (DeferredParse a) = a
getParsed (ReadyParse a) = pure a

instance DeferredParseClass (DeferredParse a) where
	finishParse (DeferredParse a) = ReadyParse <$> a
	finishParse (ReadyParse a) = pure (ReadyParse a)

instance DeferredParseClass (Maybe (DeferredParse a)) where
	finishParse Nothing = pure Nothing
	finishParse (Just v) = Just <$> finishParse v

instance DeferredParseClass [DeferredParse a] where
	finishParse v = mapM finishParse v

type AnnexOption = Parser AnnexSetter

-- Used for options that can modify Annex state by running
-- an arbitrary action in it, and can also set up AnnexRead.
data AnnexSetter = AnnexSetter
	{ annexStateSetter :: Annex ()
	, annexReadSetter :: AnnexRead -> AnnexRead
	}

instance Semigroup AnnexSetter where
	a <> b = AnnexSetter
		{ annexStateSetter = annexStateSetter a >> annexStateSetter b
		, annexReadSetter = annexReadSetter b . annexReadSetter a
		}

instance Monoid AnnexSetter where
	mempty = AnnexSetter (return ()) id

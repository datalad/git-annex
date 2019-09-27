{- adjusted branch names
 -
 - Copyright 2016-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.AdjustedBranch.Name (
	originalToAdjusted,
	adjustedToOriginal,
	AdjBranch(..),
	OrigBranch,
) where

import Types.AdjustedBranch
import Git
import qualified Git.Ref
import Utility.Misc

import Control.Applicative
import Data.List

adjustedBranchPrefix :: String
adjustedBranchPrefix = "refs/heads/adjusted/"

class SerializeAdjustment t where
	serializeAdjustment :: t -> String
	deserializeAdjustment :: String -> Maybe t

instance SerializeAdjustment Adjustment where
	serializeAdjustment (LinkAdjustment l) =
		serializeAdjustment l
	serializeAdjustment (PresenceAdjustment p Nothing) =
		serializeAdjustment p
	serializeAdjustment (PresenceAdjustment p (Just l)) = 
		serializeAdjustment p ++ "-" ++ serializeAdjustment l
	deserializeAdjustment s = 
		(LinkAdjustment <$> deserializeAdjustment s)
			<|>
		(PresenceAdjustment <$> deserializeAdjustment s1 <*> pure (deserializeAdjustment s2))
			<|>
		(PresenceAdjustment <$> deserializeAdjustment s <*> pure Nothing)
	  where
		(s1, s2) = separate (== '-') s

instance SerializeAdjustment LinkAdjustment where
	serializeAdjustment UnlockAdjustment = "unlocked"
	serializeAdjustment LockAdjustment = "locked"
	serializeAdjustment FixAdjustment = "fixed"
	serializeAdjustment UnFixAdjustment = "unfixed"
	deserializeAdjustment "unlocked" = Just UnlockAdjustment
	deserializeAdjustment "locked" = Just LockAdjustment
	deserializeAdjustment "fixed" = Just FixAdjustment
	deserializeAdjustment "unfixed" = Just UnFixAdjustment
	deserializeAdjustment _ = Nothing

instance SerializeAdjustment PresenceAdjustment where
	serializeAdjustment HideMissingAdjustment = "hidemissing"
	serializeAdjustment ShowMissingAdjustment = "showmissing"
	deserializeAdjustment "hidemissing" = Just HideMissingAdjustment
	deserializeAdjustment "showmissing" = Just ShowMissingAdjustment
	deserializeAdjustment _ = Nothing

newtype AdjBranch = AdjBranch { adjBranch :: Branch }

originalToAdjusted :: OrigBranch -> Adjustment -> AdjBranch
originalToAdjusted orig adj = AdjBranch $ Ref $
	adjustedBranchPrefix ++ base ++ '(' : serializeAdjustment adj ++ ")"
  where
	base = fromRef (Git.Ref.base orig)

type OrigBranch = Branch

adjustedToOriginal :: Branch -> Maybe (Adjustment, OrigBranch)
adjustedToOriginal b
	| adjustedBranchPrefix `isPrefixOf` bs = do
		let (base, as) = separate (== '(') (drop prefixlen bs)
		adj <- deserializeAdjustment (takeWhile (/= ')') as)
		Just (adj, Git.Ref.branchRef (Ref base))
	| otherwise = Nothing
  where
	bs = fromRef b
	prefixlen = length adjustedBranchPrefix

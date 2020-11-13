{- adjusted branch types
 -
 - Copyright 2016-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.AdjustedBranch where

data Adjustment
	= LinkAdjustment LinkAdjustment
	| PresenceAdjustment PresenceAdjustment (Maybe LinkAdjustment)
	| LinkPresentAdjustment LinkPresentAdjustment
	deriving (Show, Eq)

data LinkAdjustment
	= UnlockAdjustment
	| LockAdjustment
	| FixAdjustment
	| UnFixAdjustment
	deriving (Show, Eq)

data PresenceAdjustment
	= HideMissingAdjustment
	| ShowMissingAdjustment
	deriving (Show, Eq)

data LinkPresentAdjustment
	= UnlockPresentAdjustment
	| LockPresentAdjustment
	deriving (Show, Eq)

-- Adjustments have to be able to be reversed, so that commits made to the
-- adjusted branch can be reversed to the commit that would have been made
-- without the adjustment and applied to the original branch.
class ReversableAdjustment t where
	reverseAdjustment :: t -> t

instance ReversableAdjustment Adjustment where
	reverseAdjustment (LinkAdjustment l) = 
		LinkAdjustment (reverseAdjustment l)
	reverseAdjustment (PresenceAdjustment p ml) =
		PresenceAdjustment (reverseAdjustment p) (fmap reverseAdjustment ml)
	reverseAdjustment (LinkPresentAdjustment l) =
		LinkPresentAdjustment (reverseAdjustment l)

instance ReversableAdjustment LinkAdjustment where
	reverseAdjustment UnlockAdjustment = LockAdjustment
	-- Keep the file locked intentionally when reversing LockAdjustment.
	reverseAdjustment LockAdjustment = LockAdjustment
	reverseAdjustment FixAdjustment = UnFixAdjustment
	reverseAdjustment UnFixAdjustment = FixAdjustment

instance ReversableAdjustment PresenceAdjustment where
	reverseAdjustment HideMissingAdjustment = ShowMissingAdjustment
	reverseAdjustment ShowMissingAdjustment = HideMissingAdjustment

instance ReversableAdjustment LinkPresentAdjustment where
	reverseAdjustment UnlockPresentAdjustment = LockPresentAdjustment
	reverseAdjustment LockPresentAdjustment = UnlockPresentAdjustment

adjustmentHidesFiles :: Adjustment -> Bool
adjustmentHidesFiles (PresenceAdjustment HideMissingAdjustment _) = True
adjustmentHidesFiles _ = False


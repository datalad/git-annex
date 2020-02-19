{- proposed and accepted values
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.ProposedAccepted where

import Test.QuickCheck

-- | A value that may be proposed, or accepted.
--
-- When parsing/validating the value, may want to error out on invalid
-- input. But if a previous version of git-annex accepted an invalid value,
-- it's too late to error out, and instead the bad value may be ignored.
data ProposedAccepted t = Proposed t | Accepted t
	deriving (Show)

fromProposedAccepted :: ProposedAccepted t -> t
fromProposedAccepted (Proposed t) = t
fromProposedAccepted (Accepted t) = t

-- | Whether a value is proposed or accepted does not matter when checking
-- equality.
instance Eq t => Eq (ProposedAccepted t) where
	a == b = fromProposedAccepted a == fromProposedAccepted b

-- | Order by the contained value, not by whether it's proposed or
-- accepted.
instance Ord t => Ord (ProposedAccepted t) where
	compare a b = compare (fromProposedAccepted a) (fromProposedAccepted b)

instance Arbitrary t => Arbitrary (ProposedAccepted t) where
	arbitrary = oneof
		[ Proposed <$> arbitrary
		, Accepted <$> arbitrary
		]

{- git-annex trust levels
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.TrustLevel (
	TrustLevel(..),
	TrustMap,
	readTrustLevel,
	showTrustLevel,
	prop_read_show_TrustLevel
) where

import qualified Data.Map as M

import Types.UUID

-- This order may seem backwards, but we generally want to list dead
-- remotes last and trusted ones first.
data TrustLevel = Trusted | SemiTrusted | UnTrusted | DeadTrusted
	deriving (Eq, Enum, Ord, Bounded)

type TrustMap = M.Map UUID TrustLevel

readTrustLevel :: String -> Maybe TrustLevel
readTrustLevel "trusted" = Just Trusted
readTrustLevel "untrusted" = Just UnTrusted
readTrustLevel "semitrusted" = Just SemiTrusted
readTrustLevel "dead" = Just DeadTrusted
readTrustLevel _ = Nothing

showTrustLevel :: TrustLevel -> String
showTrustLevel Trusted = "trusted"
showTrustLevel UnTrusted = "untrusted"
showTrustLevel SemiTrusted = "semitrusted"
showTrustLevel DeadTrusted = "dead"

prop_read_show_TrustLevel :: Bool
prop_read_show_TrustLevel = all check [minBound .. maxBound]
  where
	check l = readTrustLevel (showTrustLevel l) == Just l

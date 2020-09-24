{- git-annex limits by wanted status
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Limit.Wanted where

import Annex.Common
import Annex.Wanted
import Limit
import Types.FileMatcher
import Logs.PreferredContent

addWantGet :: Annex ()
addWantGet = addPreferredContentLimit $
	checkWant $ wantGet False Nothing

addWantDrop :: Annex ()
addWantDrop = addPreferredContentLimit $
	checkWant $ wantDrop False Nothing Nothing

addPreferredContentLimit :: (MatchInfo -> Annex Bool) -> Annex ()
addPreferredContentLimit a = do
	nfn <- introspectPreferredRequiredContent matchNeedsFileName Nothing
	nfc <- introspectPreferredRequiredContent matchNeedsFileContent Nothing
	nk <- introspectPreferredRequiredContent matchNeedsKey Nothing
	addLimit $ Right $ MatchFiles
		{ matchAction = const a
		, matchNeedsFileName = nfn
		, matchNeedsFileContent = nfc
		, matchNeedsKey = nk
		}

checkWant :: (AssociatedFile -> Annex Bool) -> MatchInfo -> Annex Bool
checkWant a (MatchingFile fi) = a (AssociatedFile (Just $ matchFile fi))
checkWant a (MatchingKey _ af) = a af
checkWant _ (MatchingInfo {}) = return False

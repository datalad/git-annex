{- git-annex limits by wanted status
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Limit.Wanted where

import Annex.Common
import Annex.Wanted
import Limit
import Types.FileMatcher

addWantGet :: Annex ()
addWantGet = addLimit $ Right $ MatchFiles
	{ matchAction = const $ checkWant $ wantGet False Nothing
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	}

addWantDrop :: Annex ()
addWantDrop = addLimit $ Right $ MatchFiles
	{ matchAction = const $ checkWant $ wantDrop False Nothing Nothing
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	}

checkWant :: (AssociatedFile -> Annex Bool) -> MatchInfo -> Annex Bool
checkWant a (MatchingFile fi) = a (AssociatedFile (Just $ matchFile fi))
checkWant a (MatchingKey _ af) = a af
checkWant _ (MatchingInfo {}) = return False

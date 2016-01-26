{- git-annex limits by wanted status
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit.Wanted where

import Annex.Common
import Annex.Wanted
import Limit
import Types.FileMatcher

addWantGet :: Annex ()
addWantGet = addLimit $ Right $ const $ checkWant $ wantGet False Nothing

addWantDrop :: Annex ()
addWantDrop = addLimit $ Right $ const $ checkWant $ wantDrop False Nothing Nothing

checkWant :: (Maybe FilePath -> Annex Bool) -> MatchInfo -> Annex Bool
checkWant a (MatchingFile fi) = a (Just $ matchFile fi)
checkWant _ (MatchingKey _) = return False
checkWant _ (MatchingInfo {}) = return False

{- git-annex limits by wanted status
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit.Wanted where

import Common.Annex
import Annex.Wanted
import Limit
import Types.FileMatcher

addWantGet :: Annex ()
addWantGet = addLimit $ Right $ const $ checkWant $ wantGet False

addWantDrop :: Annex ()
addWantDrop = addLimit $ Right $ const $ checkWant $ wantDrop False Nothing

checkWant :: (Maybe FilePath -> Annex Bool) -> MatchInfo -> Annex Bool
checkWant a (MatchingFile fi) = a (Just $ matchFile fi)
checkWant _ (MatchingKey _) = return False

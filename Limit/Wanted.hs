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
addWantGet = addLimit $ Right $ const $
	\fileinfo -> wantGet False (Just $ matchFile fileinfo)

addWantDrop :: Annex ()
addWantDrop = addLimit $ Right $ const $
	\fileinfo -> wantDrop False Nothing (Just $ matchFile fileinfo)

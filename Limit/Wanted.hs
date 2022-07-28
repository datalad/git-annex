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
import qualified Remote

addWantGet :: Annex ()
addWantGet = addPreferredContentLimit $
	checkWant $ wantGet False Nothing

addWantGetBy :: String -> Annex ()
addWantGetBy name = do
	u <- Remote.nameToUUID name
	addPreferredContentLimit $ checkWant $ \af ->
		wantGetBy False Nothing af u

addWantDrop :: Annex ()
addWantDrop = addPreferredContentLimit $ checkWant $ \af ->
	wantDrop False Nothing Nothing af (Just [])

addWantDropBy :: String -> Annex ()
addWantDropBy name = do
	u <- Remote.nameToUUID name
	addPreferredContentLimit $ checkWant $ \af ->
		wantDrop False (Just u) Nothing af (Just [])

addPreferredContentLimit :: (MatchInfo -> Annex Bool) -> Annex ()
addPreferredContentLimit a = do
	nfn <- introspectPreferredRequiredContent matchNeedsFileName Nothing
	nfc <- introspectPreferredRequiredContent matchNeedsFileContent Nothing
	nk <- introspectPreferredRequiredContent matchNeedsKey Nothing
	nl <- introspectPreferredRequiredContent matchNeedsLocationLog Nothing
	addLimit $ Right $ MatchFiles
		{ matchAction = const a
		, matchNeedsFileName = nfn
		, matchNeedsFileContent = nfc
		, matchNeedsKey = nk
		, matchNeedsLocationLog = nl
		}

checkWant :: (AssociatedFile -> Annex Bool) -> MatchInfo -> Annex Bool
checkWant a (MatchingFile fi) = a (AssociatedFile (Just $ matchFile fi))
checkWant a (MatchingInfo p) = a (AssociatedFile (providedFilePath p))
checkWant _ (MatchingUserInfo {}) = return False

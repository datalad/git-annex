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
addWantGet = addPreferredContentLimit "want-get" $
	checkWant $ wantGet NoLiveUpdate False Nothing

addWantGetBy :: String -> Annex ()
addWantGetBy name = do
	u <- Remote.nameToUUID name
	addPreferredContentLimit "want-get-by" $ checkWant $ \af ->
		wantGetBy NoLiveUpdate False Nothing af u

addWantDrop :: Annex ()
addWantDrop = addPreferredContentLimit "want-drop" $ checkWant $ \af ->
	wantDrop NoLiveUpdate False Nothing Nothing af (Just [])

addWantDropBy :: String -> Annex ()
addWantDropBy name = do
	u <- Remote.nameToUUID name
	addPreferredContentLimit "want-drop-by" $ checkWant $ \af ->
		wantDrop NoLiveUpdate False (Just u) Nothing af (Just [])

addPreferredContentLimit :: String -> (MatchInfo -> Annex Bool) -> Annex ()
addPreferredContentLimit desc a = do
	nfn <- introspectPreferredRequiredContent matchNeedsFileName Nothing
	nfc <- introspectPreferredRequiredContent matchNeedsFileContent Nothing
	nk <- introspectPreferredRequiredContent matchNeedsKey Nothing
	nl <- introspectPreferredRequiredContent matchNeedsLocationLog Nothing
	lsz <- introspectPreferredRequiredContent matchNeedsLiveRepoSize Nothing
	nu <- introspectPreferredRequiredContent matchNegationUnstable Nothing
	addLimit $ Right $ MatchFiles
		{ matchAction = const $ const a
		, matchNeedsFileName = nfn
		, matchNeedsFileContent = nfc
		, matchNeedsKey = nk
		, matchNeedsLocationLog = nl
		, matchNeedsLiveRepoSize = lsz
		, matchNegationUnstable = nu
		, matchDesc = matchDescSimple desc
		}

checkWant :: (AssociatedFile -> Annex Bool) -> MatchInfo -> Annex Bool
checkWant a (MatchingFile fi) = a (AssociatedFile (Just $ matchFile fi))
checkWant a (MatchingInfo p) = a (AssociatedFile (providedFilePath p))
checkWant _ (MatchingUserInfo {}) = return False

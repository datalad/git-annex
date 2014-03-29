{- git-annex preferred content matcher configuration
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent (
	preferredContentLog,
	preferredContentSet,
	groupPreferredContentSet,
	isPreferredContent,
	preferredContentMap,
	preferredContentMapLoad,
	preferredContentMapRaw,
	groupPreferredContentMapRaw,
	checkPreferredContentExpression,
	setStandardGroup,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

import Common.Annex
import Logs.PreferredContent.Raw
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Utility.Matcher hiding (tokens)
import Annex.FileMatcher
import Annex.UUID
import Types.Group
import Types.Remote (RemoteConfig)
import Logs.Group
import Logs.Remote
import Types.FileMatcher
import Types.StandardGroups
import Limit

{- Checks if a file is preferred content for the specified repository
 - (or the current repository if none is specified). -}
isPreferredContent :: Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool
isPreferredContent mu notpresent mkey afile def = do
	u <- maybe getUUID return mu
	m <- preferredContentMap
	case M.lookup u m of
		Nothing -> return def
		Just matcher -> checkMatcher matcher mkey afile notpresent def

{- The map is cached for speed. -}
preferredContentMap :: Annex (FileMatcherMap Annex)
preferredContentMap = maybe preferredContentMapLoad return
	=<< Annex.getState Annex.preferredcontentmap

{- Loads the map, updating the cache. -}
preferredContentMapLoad :: Annex (FileMatcherMap Annex)
preferredContentMapLoad = do
	groupmap <- groupMap
	configmap <- readRemoteLog
	groupwantedmap <- groupPreferredContentMapRaw
	m <- simpleMap
		. parseLogWithUUID ((Just .) . makeMatcher groupmap configmap groupwantedmap)
		<$> Annex.Branch.get preferredContentLog
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Just m }
	return m

{- This intentionally never fails, even on unparsable expressions,
 - because the configuration is shared among repositories and newer
 - versions of git-annex may add new features. -}
makeMatcher
	:: GroupMap
	-> M.Map UUID RemoteConfig
	-> M.Map Group PreferredContentExpression
	-> UUID
	-> PreferredContentExpression
	-> FileMatcher Annex
makeMatcher groupmap configmap groupwantedmap u = go True True
  where
	go expandstandard expandgroupwanted expr
		| null (lefts tokens) = generate $ rights tokens
		| otherwise = unknownMatcher u
	  where
		tokens = exprParser matchstandard matchgroupwanted groupmap configmap (Just u) expr
		matchstandard
			| expandstandard = maybe (unknownMatcher u) (go False False)
				(standardPreferredContent <$> getStandardGroup mygroups)
			| otherwise = unknownMatcher u
		matchgroupwanted
			| expandgroupwanted = maybe (unknownMatcher u) (go True False)
				(groupwanted mygroups)
			| otherwise = unknownMatcher u
		mygroups = fromMaybe S.empty (u `M.lookup` groupsByUUID groupmap)
		groupwanted s = case M.elems $ M.filterWithKey (\k _ -> S.member k s) groupwantedmap of
			[pc] -> Just pc
			_ -> Nothing

{- When a preferred content expression cannot be parsed, but is already
 - in the log (eg, put there by a newer version of git-annex),
 - the fallback behavior is to match only files that are currently present.
 -
 - This avoid unwanted/expensive changes to the content, until the problem
 - is resolved. -}
unknownMatcher :: UUID -> FileMatcher Annex
unknownMatcher u = generate [present]
  where
	present = Operation $ matchPresent (Just u)

{- Checks if an expression can be parsed, if not returns Just error -}
checkPreferredContentExpression :: PreferredContentExpression -> Maybe String
checkPreferredContentExpression expr = case parsedToMatcher tokens of
	Left e -> Just e
	Right _ -> Nothing
  where
	tokens = exprParser matchAll matchAll emptyGroupMap M.empty Nothing expr

{- Puts a UUID in a standard group, and sets its preferred content to use
 - the standard expression for that group, unless something is already set. -}
setStandardGroup :: UUID -> StandardGroup -> Annex ()
setStandardGroup u g = do
	groupSet u $ S.singleton $ fromStandardGroup g
	m <- preferredContentMap
	unless (isJust $ M.lookup u m) $
		preferredContentSet u "standard"

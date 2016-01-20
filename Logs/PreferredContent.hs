{- git-annex preferred content matcher configuration
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent (
	preferredContentSet,
	requiredContentSet,
	groupPreferredContentSet,
	isPreferredContent,
	isRequiredContent,
	preferredContentMap,
	preferredContentMapRaw,
	requiredContentMap,
	requiredContentMapRaw,
	groupPreferredContentMapRaw,
	checkPreferredContentExpression,
	setStandardGroup,
	defaultStandardGroup,
	preferredRequiredMapsLoad,
	prop_standardGroups_parse,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

import Annex.Common
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
isPreferredContent = checkMap preferredContentMap

isRequiredContent :: Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool
isRequiredContent = checkMap requiredContentMap

checkMap :: Annex (FileMatcherMap Annex) -> Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool
checkMap getmap mu notpresent mkey afile d = do
	u <- maybe getUUID return mu
	m <- getmap
	case M.lookup u m of
		Nothing -> return d
		Just matcher -> checkMatcher matcher mkey afile notpresent d

preferredContentMap :: Annex (FileMatcherMap Annex)
preferredContentMap = maybe (fst <$> preferredRequiredMapsLoad) return
	=<< Annex.getState Annex.preferredcontentmap

requiredContentMap :: Annex (FileMatcherMap Annex)
requiredContentMap = maybe (snd <$> preferredRequiredMapsLoad) return
	=<< Annex.getState Annex.requiredcontentmap

preferredRequiredMapsLoad :: Annex (FileMatcherMap Annex, FileMatcherMap Annex)
preferredRequiredMapsLoad = do
	groupmap <- groupMap
	configmap <- readRemoteLog
	let genmap l gm = simpleMap
		. parseLogWithUUID ((Just .) . makeMatcher groupmap configmap gm)
		<$> Annex.Branch.get l
	pc <- genmap preferredContentLog =<< groupPreferredContentMapRaw
	rc <- genmap requiredContentLog M.empty
	-- Required content is implicitly also preferred content, so
	-- combine.
	let m = M.unionWith combineMatchers pc rc
	Annex.changeState $ \s -> s
		{ Annex.preferredcontentmap = Just m
		, Annex.requiredcontentmap = Just rc
		}
	return (m, rc)

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
		tokens = exprParser matchstandard matchgroupwanted (pure groupmap) configmap (Just u) expr
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
	tokens = exprParser matchAll matchAll (pure emptyGroupMap) M.empty Nothing expr

{- Puts a UUID in a standard group, and sets its preferred content to use
 - the standard expression for that group (unless preferred content is
 - already set). -}
setStandardGroup :: UUID -> StandardGroup -> Annex ()
setStandardGroup u g = do
	groupSet u $ S.singleton $ fromStandardGroup g
	unlessM (isJust . M.lookup u <$> preferredContentMap) $
		preferredContentSet u "standard"

{- Avoids overwriting the UUID's standard group or preferred content
 - when it's already been configured. -}
defaultStandardGroup :: UUID -> StandardGroup -> Annex ()
defaultStandardGroup u g = 
	unlessM (hasgroup <||> haspc) $
		setStandardGroup u g
  where
	hasgroup = not . S.null <$> lookupGroups u
	haspc = isJust . M.lookup u <$> preferredContentMap

prop_standardGroups_parse :: Bool
prop_standardGroups_parse = 
	all (isNothing . checkPreferredContentExpression . standardPreferredContent)
		[ minBound .. maxBound]

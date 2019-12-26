{- git-annex preferred content matcher configuration
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
	preferredRequiredMapsLoad',
	prop_standardGroups_parse,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import qualified Data.Attoparsec.ByteString.Lazy as A

import Annex.Common
import Logs.PreferredContent.Raw
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Utility.Matcher
import Annex.FileMatcher
import Annex.UUID
import Types.Group
import Types.Remote (RemoteConfig)
import Logs.Group
import Logs.Remote
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
		Just matcher -> checkMatcher matcher mkey afile notpresent (return d) (return d)

preferredContentMap :: Annex (FileMatcherMap Annex)
preferredContentMap = maybe (fst <$> preferredRequiredMapsLoad preferredContentTokens) return
	=<< Annex.getState Annex.preferredcontentmap

requiredContentMap :: Annex (FileMatcherMap Annex)
requiredContentMap = maybe (snd <$> preferredRequiredMapsLoad preferredContentTokens) return
	=<< Annex.getState Annex.requiredcontentmap

preferredRequiredMapsLoad :: (PreferredContentData -> [ParseToken (MatchFiles Annex)]) -> Annex (FileMatcherMap Annex, FileMatcherMap Annex)
preferredRequiredMapsLoad mktokens = do
	(pc, rc) <- preferredRequiredMapsLoad' mktokens
	let pc' = handleunknown pc
	let rc' = handleunknown rc
	Annex.changeState $ \s -> s
		{ Annex.preferredcontentmap = Just pc'
		, Annex.requiredcontentmap = Just rc'
		}
	return (pc', rc')
  where
	handleunknown = M.mapWithKey $ \u ->
		either (const $ unknownMatcher u) id

preferredRequiredMapsLoad' :: (PreferredContentData -> [ParseToken (MatchFiles Annex)]) -> Annex (M.Map UUID (Either String (FileMatcher Annex)), M.Map UUID (Either String (FileMatcher Annex)))
preferredRequiredMapsLoad' mktokens = do
	groupmap <- groupMap
	configmap <- readRemoteLog
	let genmap l gm = 
		let mk u = makeMatcher groupmap configmap gm u mktokens
		in simpleMap
			. parseLogOldWithUUID (\u -> mk u . decodeBS <$> A.takeByteString)
			<$> Annex.Branch.get l
	pc <- genmap preferredContentLog =<< groupPreferredContentMapRaw
	rc <- genmap requiredContentLog M.empty
	-- Required content is implicitly also preferred content, so combine.
	let pc' = M.unionWith combiner pc rc
	return (pc', rc)
  where
	combiner (Right a) (Right b) = Right (combineMatchers a b)
	combiner (Left a)  (Left b)  = Left (a ++ " " ++ b)
	combiner (Left a)  (Right _) = Left a
	combiner (Right _) (Left b)  = Left b

{- This intentionally never fails, even on unparsable expressions,
 - because the configuration is shared among repositories and newer
 - versions of git-annex may add new features. -}
makeMatcher
	:: GroupMap
	-> M.Map UUID RemoteConfig
	-> M.Map Group PreferredContentExpression
	-> UUID
	-> (PreferredContentData -> [ParseToken (MatchFiles Annex)])
	-> PreferredContentExpression
	-> Either String (FileMatcher Annex)
makeMatcher groupmap configmap groupwantedmap u mktokens = go True True
  where
	go expandstandard expandgroupwanted expr
		| null (lefts tokens) = Right $ generate $ rights tokens
		| otherwise = Left (unwords (lefts tokens))
	  where
		tokens = preferredContentParser (mktokens pcd) expr
		pcd = PCD
			{ matchStandard = matchstandard
			, matchGroupWanted = matchgroupwanted
			, getGroupMap = pure groupmap
			, configMap = configmap
			, repoUUID = Just u
			}
		matchstandard
			| expandstandard = maybe (Right $ unknownMatcher u) (go False False)
				(standardPreferredContent <$> getStandardGroup mygroups)
			| otherwise = Right $ unknownMatcher u
		matchgroupwanted
			| expandgroupwanted = maybe (Right $ unknownMatcher u) (go True False)
				(groupwanted mygroups)
			| otherwise = Right $ unknownMatcher u
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
	present = Operation $ limitPresent (Just u)

{- Checks if an expression can be parsed, if not returns Just error -}
checkPreferredContentExpression :: PreferredContentExpression -> Maybe String
checkPreferredContentExpression expr = case parsedToMatcher tokens of
	Left e -> Just e
	Right _ -> Nothing
  where
	tokens = preferredContentParser (preferredContentTokens pcd) expr
	pcd = PCD
		{ matchStandard = Right matchAll
		, matchGroupWanted = Right matchAll
		, getGroupMap = pure emptyGroupMap
		, configMap = M.empty
		, repoUUID = Nothing
		}

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

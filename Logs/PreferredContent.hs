{- git-annex preferred content matcher configuration
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
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
	introspectPreferredRequiredContent,
	prop_standardGroups_parse,
) where

import Annex.Common
import Logs.PreferredContent.Raw
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Utility.Matcher
import Annex.FileMatcher
import Annex.UUID
import Logs.Group
import Logs.Remote
import Types.StandardGroups
import Limit

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Attoparsec.ByteString.Lazy as A

{- Checks if a file is preferred content (or required content) for the
 - specified repository (or the current repository if none is specified). -}
isPreferredContent :: LiveUpdate -> Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool
isPreferredContent = checkMap preferredContentMap

isRequiredContent :: LiveUpdate -> Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool
isRequiredContent = checkMap requiredContentMap

checkMap :: Annex (FileMatcherMap Annex) -> LiveUpdate -> Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool
checkMap getmap lu mu notpresent mkey afile d = do
	u <- maybe getUUID return mu
	m <- getmap
	case M.lookup u m of
		Nothing -> return d
		Just matcher -> checkMatcher matcher mkey afile lu notpresent (return d) (return d)

{- Checks if the preferred or required content for the specified repository
 - (or the current repository if none is specified) contains any terms
 - that meet the condition. -}
introspectPreferredRequiredContent :: (MatchFiles Annex -> Bool) -> Maybe UUID -> Annex Bool
introspectPreferredRequiredContent c mu = do
	u <- maybe getUUID return mu
	check u preferredContentMap <||> check u requiredContentMap
  where
	check u mk = mk >>= return . maybe False (any c . fst) . M.lookup u

preferredContentMap :: Annex (FileMatcherMap Annex)
preferredContentMap = maybe (fst <$> preferredRequiredMapsLoad preferredContentTokens) return
	=<< Annex.getState Annex.preferredcontentmap

requiredContentMap :: Annex (FileMatcherMap Annex)
requiredContentMap = maybe (snd <$> preferredRequiredMapsLoad preferredContentTokens) return
	=<< Annex.getState Annex.requiredcontentmap

preferredRequiredMapsLoad :: (PreferredContentData -> [ParseToken (MatchFiles Annex)]) -> Annex (FileMatcherMap Annex, FileMatcherMap Annex)
preferredRequiredMapsLoad mktokens = do
	(pc, rc) <- preferredRequiredMapsLoad' id mktokens
	let pc' = handleunknown (MatcherDesc "preferred content") pc
	let rc' = handleunknown (MatcherDesc "required content") rc
	Annex.changeState $ \s -> s
		{ Annex.preferredcontentmap = Just pc'
		, Annex.requiredcontentmap = Just rc'
		}
	return (pc', rc')
  where
	handleunknown matcherdesc = M.mapWithKey $ \u v ->
		(either (const $ unknownMatcher u) id v, matcherdesc)

preferredRequiredMapsLoad' :: (Matcher (MatchFiles Annex) -> Matcher (MatchFiles Annex)) -> (PreferredContentData -> [ParseToken (MatchFiles Annex)]) -> Annex (M.Map UUID (Either String (Matcher (MatchFiles Annex))), M.Map UUID (Either String (Matcher (MatchFiles Annex))))
preferredRequiredMapsLoad' matcherf mktokens = do
	groupmap <- groupMap
	configmap <- remoteConfigMap
	let genmap l gm = 
		let mk u = makeMatcher groupmap configmap
			gm u matcherf mktokens (Right (unknownMatcher u))
		in simpleMap
			. parseLogOldWithUUID (\u -> mk u . decodeBS <$> A.takeByteString)
			<$> Annex.Branch.get l
	gm <- groupPreferredContentMapRaw
	pc <- genmap preferredContentLog gm
	rc <- genmap requiredContentLog gm
	-- Required content is implicitly also preferred content, so combine.
	let pc' = M.unionWith combiner pc rc
	return (pc', rc)
  where
	combiner (Right a) (Right b) = Right (combineMatchers a b)
	combiner (Left a)  (Left b)  = Left (a ++ " " ++ b)
	combiner (Left a)  (Right _) = Left a
	combiner (Right _) (Left b)  = Left b

{- Parsing preferred content expressions intentionally never fails,
 - because the configuration is shared among repositories and newer
 - versions of git-annex may add new features.
 -
 - When a preferred content expression cannot be parsed, but is already
 - in the log (eg, put there by a newer version of git-annex),
 - the fallback behavior is to match only files that are currently present.
 -
 - This avoid unwanted/expensive changes to the content, until the problem
 - is resolved. -}
unknownMatcher :: UUID -> Matcher (MatchFiles Annex)
unknownMatcher u = generate [present]
  where
	present = Operation $ limitPresent (Just u)

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

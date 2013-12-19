{- git-annex preferred content matcher configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent (
	preferredContentLog,
	preferredContentSet,
	isPreferredContent,
	preferredContentMap,
	preferredContentMapLoad,
	preferredContentMapRaw,
	checkPreferredContentExpression,
	setStandardGroup,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Time.Clock.POSIX

import Common.Annex
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import qualified Utility.Matcher
import Annex.FileMatcher
import Annex.UUID
import Types.Limit
import Types.Group
import Types.Remote (RemoteConfig)
import Logs.Group
import Logs.Remote
import Types.StandardGroups

{- Changes the preferred content configuration of a remote. -}
preferredContentSet :: UUID -> String -> Annex ()
preferredContentSet uuid@(UUID _) val = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change preferredContentLog $
		showLog id . changeLog ts uuid val . parseLog Just
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Nothing }
preferredContentSet NoUUID _ = error "unknown UUID; cannot modify"

{- Checks if a file is preferred content for the specified repository
 - (or the current repository if none is specified). -}
isPreferredContent :: Maybe UUID -> AssumeNotPresent -> FilePath -> Bool -> Annex Bool
isPreferredContent mu notpresent file def = do
	u <- maybe getUUID return mu
	m <- preferredContentMap
	case M.lookup u m of
		Nothing -> return def
		Just matcher -> checkFileMatcher' matcher file notpresent def

{- The map is cached for speed. -}
preferredContentMap :: Annex Annex.PreferredContentMap
preferredContentMap = maybe preferredContentMapLoad return
	=<< Annex.getState Annex.preferredcontentmap

{- Loads the map, updating the cache. -}
preferredContentMapLoad :: Annex Annex.PreferredContentMap
preferredContentMapLoad = do
	groupmap <- groupMap
	configmap <- readRemoteLog
	m <- simpleMap
		. parseLogWithUUID ((Just .) . makeMatcher groupmap configmap)
		<$> Annex.Branch.get preferredContentLog
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Just m }
	return m

preferredContentMapRaw :: Annex (M.Map UUID String)
preferredContentMapRaw = simpleMap . parseLog Just
	<$> Annex.Branch.get preferredContentLog

{- This intentionally never fails, even on unparsable expressions,
 - because the configuration is shared among repositories and newer
 - versions of git-annex may add new features. Instead, parse errors
 - result in a Matcher that will always succeed. -}
makeMatcher :: GroupMap -> M.Map UUID RemoteConfig -> UUID -> String -> FileMatcher
makeMatcher groupmap configmap u expr
	| expr == "standard" = standardMatcher groupmap configmap u
	| null (lefts tokens) = Utility.Matcher.generate $ rights tokens
	| otherwise = matchAll
  where
	tokens = exprParser groupmap configmap (Just u) expr

{- Standard matchers are pre-defined for some groups. If none is defined,
 - or a repository is in multiple groups with standard matchers, match all. -}
standardMatcher :: GroupMap -> M.Map UUID RemoteConfig -> UUID -> FileMatcher
standardMatcher groupmap configmap u = 
	maybe matchAll (makeMatcher groupmap configmap u . preferredContent) $
		getStandardGroup =<< u `M.lookup` groupsByUUID groupmap

{- Checks if an expression can be parsed, if not returns Just error -}
checkPreferredContentExpression :: String -> Maybe String
checkPreferredContentExpression expr
	| expr == "standard" = Nothing
	| otherwise = case parsedToMatcher tokens of
		Left e -> Just e
		Right _ -> Nothing
  where
	tokens = exprParser emptyGroupMap M.empty Nothing expr

{- Puts a UUID in a standard group, and sets its preferred content to use
 - the standard expression for that group, unless something is already set. -}
setStandardGroup :: UUID -> StandardGroup -> Annex ()
setStandardGroup u g = do
	groupSet u $ S.singleton $ fromStandardGroup g
	m <- preferredContentMap
	unless (isJust $ M.lookup u m) $
		preferredContentSet u "standard"

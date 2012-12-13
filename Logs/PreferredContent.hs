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
import Logs.UUIDBased
import Limit
import qualified Utility.Matcher
import Annex.UUID
import Git.FilePath
import Types.Group
import Logs.Group
import Types.StandardGroups

{- Filename of preferred-content.log. -}
preferredContentLog :: FilePath
preferredContentLog = "preferred-content.log"

{- Changes the preferred content configuration of a remote. -}
preferredContentSet :: UUID -> String -> Annex ()
preferredContentSet uuid@(UUID _) val = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change preferredContentLog $
		showLog id . changeLog ts uuid val . parseLog Just
	Annex.changeState $ \s -> s { Annex.groupmap = Nothing }
preferredContentSet NoUUID _ = error "unknown UUID; cannot modify"

{- Checks if a file is preferred content for the specified repository
 - (or the current repository if none is specified). -}
isPreferredContent :: Maybe UUID -> AssumeNotPresent -> FilePath -> Bool -> Annex Bool
isPreferredContent mu notpresent file def = do
	matchfile <- getTopFilePath <$> inRepo (toTopFilePath file)
	let fi = Annex.FileInfo
		{ Annex.matchFile = matchfile
		, Annex.relFile = file
		}
	u <- maybe getUUID return mu
	m <- preferredContentMap
	case M.lookup u m of
		Nothing -> return def
		Just matcher
			| Utility.Matcher.isEmpty matcher -> return def
			| otherwise -> Utility.Matcher.matchMrun matcher $ 
				\a -> a notpresent fi

{- The map is cached for speed. -}
preferredContentMap :: Annex Annex.PreferredContentMap
preferredContentMap = maybe preferredContentMapLoad return
	=<< Annex.getState Annex.preferredcontentmap

{- Loads the map, updating the cache. -}
preferredContentMapLoad :: Annex Annex.PreferredContentMap
preferredContentMapLoad = do
	groupmap <- groupMap
	m <- simpleMap
		. parseLogWithUUID ((Just .) . makeMatcher groupmap)
		<$> Annex.Branch.get preferredContentLog
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Just m }
	return m

preferredContentMapRaw :: Annex (M.Map UUID String)
preferredContentMapRaw = simpleMap . parseLog Just
	<$> Annex.Branch.get preferredContentLog

{- This intentionally never fails, even on unparsable expressions,
 - because the configuration is shared amoung repositories and newer
 - versions of git-annex may add new features. Instead, parse errors
 - result in a Matcher that will always succeed. -}
makeMatcher :: GroupMap -> UUID -> String -> Utility.Matcher.Matcher MatchFiles
makeMatcher groupmap u s
	| s == "standard" = standardMatcher groupmap u
	| null (lefts tokens) = Utility.Matcher.generate $ rights tokens
	| otherwise = matchAll
  where
	tokens = map (parseToken (Just u) groupmap) (tokenizeMatcher s)

{- Standard matchers are pre-defined for some groups. If none is defined,
 - or a repository is in multiple groups with standard matchers, match all. -}
standardMatcher :: GroupMap -> UUID -> Utility.Matcher.Matcher MatchFiles
standardMatcher m u = maybe matchAll (makeMatcher m u . preferredContent) $
	getStandardGroup =<< u `M.lookup` groupsByUUID m

matchAll :: Utility.Matcher.Matcher MatchFiles
matchAll = Utility.Matcher.generate []

{- Checks if an expression can be parsed, if not returns Just error -}
checkPreferredContentExpression :: String -> Maybe String
checkPreferredContentExpression s
	| s == "standard" = Nothing
	| otherwise = case lefts $ map (parseToken Nothing emptyGroupMap) (tokenizeMatcher s) of
		[] -> Nothing
		l -> Just $ unwords $ map ("Parse failure: " ++) l

parseToken :: (Maybe UUID) -> GroupMap -> String -> Either String (Utility.Matcher.Token MatchFiles)
parseToken mu groupmap t
	| any (== t) Utility.Matcher.tokens = Right $ Utility.Matcher.token t
	| t == "present" = use $ limitPresent mu
	| otherwise = maybe (Left $ "near " ++ show t) use $ M.lookup k $
		M.fromList
			[ ("include", limitInclude)
			, ("exclude", limitExclude)
			, ("copies", limitCopies)
			, ("inbackend", limitInBackend)
			, ("largerthan", limitSize (>))
			, ("smallerthan", limitSize (<))
			, ("inallgroup", limitInAllGroup groupmap)
			]
  where
	(k, v) = separate (== '=') t
	use a = Utility.Matcher.Operation <$> a v

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null ) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

{- Puts a UUID in a standard group, and sets its preferred content to use
 - the standard expression for that group, unless something is already set. -}
setStandardGroup :: UUID -> StandardGroup -> Annex ()
setStandardGroup u g = do
	groupSet u $ S.singleton $ fromStandardGroup g
	m <- preferredContentMap
	unless (isJust $ M.lookup u m) $
		preferredContentSet u "standard"

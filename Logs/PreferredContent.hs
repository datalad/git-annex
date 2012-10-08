{- git-annex preferred content matcher configuration
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent (
	preferredContentSet,
	isPreferredContent,
	preferredContentMap,
	preferredContentMapRaw,
	checkPreferredContentExpression,
) where

import qualified Data.Map as M
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
isPreferredContent :: Maybe UUID -> AssumeNotPresent -> TopFilePath -> Annex Bool
isPreferredContent mu notpresent file = do
	u <- maybe getUUID return mu
	m <- preferredContentMap
	case M.lookup u m of
		Nothing -> return True
		Just matcher ->
			Utility.Matcher.matchM2 matcher notpresent $
				getTopFilePath file

{- Read the preferredContentLog into a map. The map is cached for speed. -}
preferredContentMap :: Annex Annex.PreferredContentMap
preferredContentMap = do
	groupmap <- groupMap
	cached <- Annex.getState Annex.preferredcontentmap
	case cached of
		Just m -> return m
		Nothing -> do
			m <- simpleMap . parseLog (Just . makeMatcher groupmap)
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
makeMatcher :: GroupMap -> String -> Utility.Matcher.Matcher MatchFiles
makeMatcher groupmap s
	| null (lefts tokens) =  Utility.Matcher.generate $ rights tokens
 	| otherwise = Utility.Matcher.generate []
	where
		tokens = map (parseToken groupmap) (tokenizeMatcher s)

{- Checks if an expression can be parsed, if not returns Just error -}
checkPreferredContentExpression :: String -> Maybe String
checkPreferredContentExpression s = 
	case lefts $ map (parseToken emptyGroupMap) (tokenizeMatcher s) of
		[] -> Nothing
		l -> Just $ unwords $ map ("Parse failure: " ++) l

parseToken :: GroupMap -> String -> Either String (Utility.Matcher.Token MatchFiles)
parseToken groupmap t
	| any (== t) Utility.Matcher.tokens = Right $ Utility.Matcher.token t
	| otherwise = maybe (Left $ "near " ++ show t) use $ M.lookup k m
	where
		(k, v) = separate (== '=') t
		m = M.fromList
			[ ("include", limitInclude)
			, ("exclude", limitExclude)
			, ("in", limitIn)
			, ("copies", limitCopies)
			, ("inbackend", limitInBackend)
			, ("largerthan", limitSize (>))
			, ("smallerthan", limitSize (<))
			, ("ingroup", limitInGroup groupmap)
			]
		use a = Utility.Matcher.Operation <$> a v

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null ) . concatMap splitparens . words
	where
		splitparens = segmentDelim (`elem` "()")

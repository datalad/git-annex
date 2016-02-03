{- git-annex file matching
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.FileMatcher where

import qualified Data.Map as M

import Annex.Common
import Limit
import Utility.Matcher
import Types.Group
import Logs.Group
import Annex.UUID
import qualified Annex
import Types.FileMatcher
import Git.FilePath
import Types.Remote (RemoteConfig)
import Annex.CheckAttr
import Git.CheckAttr (unspecifiedAttr)

import Data.Either
import qualified Data.Set as S

type GetFileMatcher = FilePath -> Annex (FileMatcher Annex)

checkFileMatcher :: GetFileMatcher -> FilePath -> Annex Bool
checkFileMatcher getmatcher file = do
	matcher <- getmatcher file
	checkMatcher matcher Nothing (Just file) S.empty True

checkMatcher :: FileMatcher Annex -> Maybe Key -> AssociatedFile -> AssumeNotPresent -> Bool -> Annex Bool
checkMatcher matcher mkey afile notpresent d
	| isEmpty matcher = return d
	| otherwise = case (mkey, afile) of
		(_, Just file) -> go =<< fileMatchInfo file
		(Just key, _) -> go (MatchingKey key)
		_ -> return d
  where
	go mi = matchMrun matcher $ \a -> a notpresent mi

fileMatchInfo :: FilePath -> Annex MatchInfo
fileMatchInfo file = do
	matchfile <- getTopFilePath <$> inRepo (toTopFilePath file)
	return $ MatchingFile FileInfo
		{ matchFile = matchfile
		, currFile = file
		}

matchAll :: FileMatcher Annex
matchAll = generate []

parsedToMatcher :: [Either String (Token (MatchFiles Annex))] -> Either String (FileMatcher Annex)
parsedToMatcher parsed = case partitionEithers parsed of
	([], vs) -> Right $ generate vs
	(es, _) -> Left $ unwords $ map ("Parse failure: " ++) es

exprParser :: FileMatcher Annex -> FileMatcher Annex -> Annex GroupMap -> M.Map UUID RemoteConfig -> Maybe UUID -> String -> [Either String (Token (MatchFiles Annex))]
exprParser matchstandard matchgroupwanted getgroupmap configmap mu expr =
	map parse $ tokenizeMatcher expr
  where
	parse = parseToken
		matchstandard
		matchgroupwanted
		(limitPresent mu)
		(limitInDir preferreddir)
		getgroupmap
	preferreddir = fromMaybe "public" $
		M.lookup "preferreddir" =<< (`M.lookup` configmap) =<< mu

parseToken :: FileMatcher Annex -> FileMatcher Annex -> MkLimit Annex -> MkLimit Annex -> Annex GroupMap -> String -> Either String (Token (MatchFiles Annex))
parseToken matchstandard matchgroupwanted checkpresent checkpreferreddir getgroupmap t
	| t `elem` tokens = Right $ token t
	| otherwise = case t of
		"standard" -> call matchstandard
		"groupwanted" -> call matchgroupwanted
		"present" -> use checkpresent
		"inpreferreddir" -> use checkpreferreddir
		"unused" -> simply limitUnused
		"anything" -> simply limitAnything
		"nothing" -> simply limitNothing
		_ -> case k of
			"include" -> use limitInclude
			"exclude" -> use limitExclude
			"copies" -> use limitCopies
			"lackingcopies" -> use $ limitLackingCopies False
			"approxlackingcopies" -> use $ limitLackingCopies True
			"inbackend" -> use limitInBackend
			"largerthan" -> use $ limitSize (>)
			"smallerthan" -> use $ limitSize (<)
			"metadata" -> use limitMetaData
			"inallgroup" -> use $ limitInAllGroup getgroupmap
			_ -> Left $ "near " ++ show t
  where
	(k, v) = separate (== '=') t
	simply = Right . Operation
	use a = Operation <$> a v
	call sub = Right $ Operation $ \notpresent mi ->
		matchMrun sub $ \a -> a notpresent mi

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null ) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

{- Generates a matcher for files large enough (or meeting other criteria)
 - to be added to the annex, rather than directly to git. -}
largeFilesMatcher :: Annex GetFileMatcher
largeFilesMatcher = go =<< annexLargeFiles <$> Annex.getGitConfig
  where
	go (Just expr) = do
		matcher <- mkmatcher expr
		return $ const $ return matcher
	go Nothing = return $ \file -> do
		expr <- checkAttr "annex.largefiles" file
		if null expr || expr == unspecifiedAttr
			then return matchAll
			else mkmatcher expr

	mkmatcher expr = do
		u <- getUUID
		-- No need to read remote configs, that's only needed for
		-- inpreferreddir, which is used in preferred content
		-- expressions but does not make sense in the 
		-- annex.largefiles expression.
		let emptyconfig = M.empty
		either badexpr return $
			parsedToMatcher $ exprParser matchAll matchAll groupMap emptyconfig (Just u) expr
	badexpr e = error $ "bad annex.largefiles configuration: " ++ e

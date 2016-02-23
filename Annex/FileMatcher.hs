{- git-annex file matching
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.FileMatcher (
	GetFileMatcher,
	checkFileMatcher,
	checkMatcher,
	matchAll,
	preferredContentParser,
	parsedToMatcher,
	mkLargeFilesParser,
	largeFilesMatcher,
) where

import qualified Data.Map as M

import Annex.Common
import Limit
import Utility.Matcher
import Types.Group
import qualified Annex
import Types.FileMatcher
import Git.FilePath
import Types.Remote (RemoteConfig)
import Annex.CheckAttr
import Git.CheckAttr (unspecifiedAttr)

#ifdef WITH_MAGICMIME
import Magic
#endif

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

parsedToMatcher :: [ParseResult] -> Either String (FileMatcher Annex)
parsedToMatcher parsed = case partitionEithers parsed of
	([], vs) -> Right $ generate vs
	(es, _) -> Left $ unwords $ map ("Parse failure: " ++) es

data ParseToken
	= SimpleToken String ParseResult
	| ValueToken String (String -> ParseResult)

type ParseResult = Either String (Token (MatchFiles Annex))

parseToken :: [ParseToken] -> String -> ParseResult
parseToken l t
	| t `elem` tokens = Right $ token t
	| otherwise = go l
  where
	go [] = Left $ "near " ++ show t
	go (SimpleToken s r : _) | s == t = r
	go (ValueToken s mkr : _) | s == k = mkr v
	go (_ : ps) = go ps
	(k, v) = separate (== '=') t

commonTokens :: [ParseToken]
commonTokens =
	[ SimpleToken "unused" (simply limitUnused)
	, SimpleToken "anything" (simply limitAnything)
	, SimpleToken "nothing" (simply limitNothing)
	, ValueToken "include" (usev limitInclude)
	, ValueToken "exclude" (usev limitExclude)
	, ValueToken "largerthan" (usev $ limitSize (>))
	, ValueToken "smallerthan" (usev $ limitSize (<))
	]

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null ) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

preferredContentParser :: FileMatcher Annex -> FileMatcher Annex -> Annex GroupMap -> M.Map UUID RemoteConfig -> Maybe UUID -> String -> [ParseResult]
preferredContentParser matchstandard matchgroupwanted getgroupmap configmap mu expr =
	map parse $ tokenizeMatcher expr
  where
 	parse = parseToken $
		[ SimpleToken "standard" (call matchstandard)
		, SimpleToken "groupwanted" (call matchgroupwanted)
		, SimpleToken "present" (simply $ limitPresent mu)
		, SimpleToken "inpreferreddir" (simply $ limitInDir preferreddir)
		, ValueToken "copies" (usev limitCopies)
		, ValueToken "lackingcopies" (usev $ limitLackingCopies False)
		, ValueToken "approxlackingcopies" (usev $ limitLackingCopies True)
		, ValueToken "inbacked" (usev limitInBackend)
		, ValueToken "metadata" (usev limitMetaData)
		, ValueToken "inallgroup" (usev $ limitInAllGroup getgroupmap)
		] ++ commonTokens
	preferreddir = fromMaybe "public" $
		M.lookup "preferreddir" =<< (`M.lookup` configmap) =<< mu

mkLargeFilesParser :: Annex (String -> [ParseResult])
mkLargeFilesParser = do
#ifdef WITH_MAGICMIME
	magicmime <- liftIO $ catchMaybeIO $ do
		m <- magicOpen [MagicMimeType]
		liftIO $ magicLoadDefault m
		return m
#endif
	let parse = parseToken $ commonTokens
#ifdef WITH_MAGICMIME
		++ [ ValueToken "mimetype" (usev $ matchMagic magicmime) ]
#else
		++ [ ValueToken "mimetype" (const $ Left "\"mimetype\" not supported; not built with MagicMime support") ]
#endif
	return $ map parse . tokenizeMatcher

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
		parser <- mkLargeFilesParser
		either badexpr return $ parsedToMatcher $ parser expr
	badexpr e = error $ "bad annex.largefiles configuration: " ++ e

simply :: MatchFiles Annex -> ParseResult
simply = Right . Operation

usev :: MkLimit Annex -> String -> ParseResult
usev a v = Operation <$> a v

call :: FileMatcher Annex -> ParseResult
call sub = Right $ Operation $ \notpresent mi ->
	matchMrun sub $ \a -> a notpresent mi

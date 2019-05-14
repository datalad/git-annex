{- git-annex file matching
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.FileMatcher (
	GetFileMatcher,
	checkFileMatcher,
	checkFileMatcher',
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
import Annex.Magic
import Git.CheckAttr (unspecifiedAttr)

import Data.Either
import qualified Data.Set as S

type GetFileMatcher = FilePath -> Annex (FileMatcher Annex)

checkFileMatcher :: GetFileMatcher -> FilePath -> Annex Bool
checkFileMatcher getmatcher file = checkFileMatcher' getmatcher file (return True)

-- | Allows running an action when no matcher is configured for the file.
checkFileMatcher' :: GetFileMatcher -> FilePath -> Annex Bool -> Annex Bool
checkFileMatcher' getmatcher file notconfigured = do
	matcher <- getmatcher file
	checkMatcher matcher Nothing afile S.empty notconfigured d
  where
	afile = AssociatedFile (Just file)
	-- checkMatcher will never use this, because afile is provided.
	d = return True

checkMatcher :: FileMatcher Annex -> Maybe Key -> AssociatedFile -> AssumeNotPresent -> Annex Bool -> Annex Bool -> Annex Bool
checkMatcher matcher mkey afile notpresent notconfigured d
	| isEmpty matcher = notconfigured
	| otherwise = case (mkey, afile) of
		(_, AssociatedFile (Just file)) -> go =<< fileMatchInfo file
		(Just key, _) -> go (MatchingKey key afile)
		_ -> d
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

parsedToMatcher :: [ParseResult (MatchFiles Annex)] -> Either String (FileMatcher Annex)
parsedToMatcher parsed = case partitionEithers parsed of
	([], vs) -> Right $ generate vs
	(es, _) -> Left $ unwords $ map ("Parse failure: " ++) es

data ParseToken t
	= SimpleToken String (ParseResult t)
	| ValueToken String (String -> ParseResult t)

type ParseResult t = Either String (Token t)

parseToken :: [ParseToken t] -> String -> ParseResult t
parseToken l t
	| t `elem` tokens = Right $ token t
	| otherwise = go l
  where
	go [] = Left $ "near " ++ show t
	go (SimpleToken s r : _) | s == t = r
	go (ValueToken s mkr : _) | s == k = mkr v
	go (_ : ps) = go ps
	(k, v) = separate (== '=') t

commonTokens :: [ParseToken (MatchFiles Annex)]
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
tokenizeMatcher = filter (not . null) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

preferredContentParser :: FileMatcher Annex -> FileMatcher Annex -> Annex GroupMap -> M.Map UUID RemoteConfig -> Maybe UUID -> String -> [ParseResult (MatchFiles Annex)]
preferredContentParser matchstandard matchgroupwanted getgroupmap configmap mu expr =
	map parse $ tokenizeMatcher expr
  where
 	parse = parseToken $
		[ SimpleToken "standard" (call matchstandard)
		, SimpleToken "groupwanted" (call matchgroupwanted)
		, SimpleToken "present" (simply $ limitPresent mu)
		, SimpleToken "inpreferreddir" (simply $ limitInDir preferreddir)
		, SimpleToken "securehash" (simply limitSecureHash)
		, ValueToken "copies" (usev limitCopies)
		, ValueToken "lackingcopies" (usev $ limitLackingCopies False)
		, ValueToken "approxlackingcopies" (usev $ limitLackingCopies True)
		, ValueToken "inbacked" (usev limitInBackend)
		, ValueToken "metadata" (usev limitMetaData)
		, ValueToken "inallgroup" (usev $ limitInAllGroup getgroupmap)
		] ++ commonTokens
	preferreddir = fromMaybe "public" $
		M.lookup "preferreddir" =<< (`M.lookup` configmap) =<< mu

mkLargeFilesParser :: Annex (String -> [ParseResult (MatchFiles Annex)])
mkLargeFilesParser = do
	magicmime <- liftIO initMagicMime
#ifdef WITH_MAGICMIME
	let mimer n f = ValueToken n (usev $ f magicmime)
#else
	let mimer n = ValueToken n $ 
		const $ Left $ "\""++n++"\" not supported; not built with MagicMime support"
#endif
	let parse = parseToken $ commonTokens ++
#ifdef WITH_MAGICMIME
		[ mimer "mimetype" $
			matchMagic "mimetype" getMagicMimeType providedMimeType
		, mimer "mimeencoding" $
			matchMagic "mimeencoding" getMagicMimeEncoding providedMimeEncoding
		]
#else
		[ mimer "mimetype"
		, mimer "mimeencoding"
		]
#endif
	return $ map parse . tokenizeMatcher
  where

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
	badexpr e = giveup $ "bad annex.largefiles configuration: " ++ e

simply :: MatchFiles Annex -> ParseResult (MatchFiles Annex)
simply = Right . Operation

usev :: MkLimit Annex -> String -> ParseResult (MatchFiles Annex)
usev a v = Operation <$> a v

call :: FileMatcher Annex -> ParseResult (MatchFiles Annex)
call sub = Right $ Operation $ \notpresent mi ->
	matchMrun sub $ \a -> a notpresent mi

{- git-annex file matching
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.FileMatcher (
	GetFileMatcher,
	checkFileMatcher,
	checkFileMatcher',
	checkMatcher,
	checkMatcher',
	matchAll,
	PreferredContentData(..),
	preferredContentTokens,
	preferredContentKeylessTokens,
	preferredContentParser,
	ParseToken,
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
import Annex.Magic
#endif

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
	go mi = checkMatcher' matcher mi notpresent

checkMatcher' :: FileMatcher Annex -> MatchInfo -> AssumeNotPresent -> Annex Bool
checkMatcher' matcher mi notpresent =
	matchMrun matcher $ \a -> a notpresent mi

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
parseToken l t = case syntaxToken t of
	Right st -> Right st
	Left _ -> go l
  where
	go [] = Left $ "near " ++ show t
	go (SimpleToken s r : _) | s == t = r
	go (ValueToken s mkr : _) | s == k = mkr v
	go (_ : ps) = go ps
	(k, v) = separate (== '=') t

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

commonKeylessTokens :: LimitBy -> [ParseToken (MatchFiles Annex)]
commonKeylessTokens lb =
	[ SimpleToken "anything" (simply limitAnything)
	, SimpleToken "nothing" (simply limitNothing)
	, ValueToken "include" (usev limitInclude)
	, ValueToken "exclude" (usev limitExclude)
	, ValueToken "largerthan" (usev $ limitSize lb (>))
	, ValueToken "smallerthan" (usev $ limitSize lb (<))
	]

commonKeyedTokens :: [ParseToken (MatchFiles Annex)]
commonKeyedTokens = 
	[ SimpleToken "unused" (simply limitUnused)
	]

data PreferredContentData = PCD
	{ matchStandard :: Either String (FileMatcher Annex)
	, matchGroupWanted :: Either String (FileMatcher Annex)
	, getGroupMap :: Annex GroupMap
	, configMap :: M.Map UUID RemoteConfig
	, repoUUID :: Maybe UUID
	}

-- Tokens of preferred content expressions that do not need a Key to be
-- known. 
--
-- When importing from a special remote, this is used to match
-- some preferred content expressions before the content is downloaded,
-- so the Key is not known.
preferredContentKeylessTokens :: PreferredContentData -> [ParseToken (MatchFiles Annex)]
preferredContentKeylessTokens pcd =
	[ SimpleToken "standard" (call $ matchStandard pcd)
	, SimpleToken "groupwanted" (call $ matchGroupWanted pcd)
	, SimpleToken "inpreferreddir" (simply $ limitInDir preferreddir)
	] ++ commonKeylessTokens LimitAnnexFiles
  where
	preferreddir = fromMaybe "public" $
		M.lookup "preferreddir" =<< (`M.lookup` configMap pcd) =<< repoUUID pcd

preferredContentKeyedTokens :: PreferredContentData -> [ParseToken (MatchFiles Annex)]
preferredContentKeyedTokens pcd =
	[ SimpleToken "present" (simply $ limitPresent $ repoUUID pcd)
	, SimpleToken "securehash" (simply limitSecureHash)
	, ValueToken "copies" (usev limitCopies)
	, ValueToken "lackingcopies" (usev $ limitLackingCopies False)
	, ValueToken "approxlackingcopies" (usev $ limitLackingCopies True)
	, ValueToken "inbacked" (usev limitInBackend)
	, ValueToken "metadata" (usev limitMetaData)
	, ValueToken "inallgroup" (usev $ limitInAllGroup $ getGroupMap pcd)
	] ++ commonKeyedTokens

preferredContentTokens :: PreferredContentData -> [ParseToken (MatchFiles Annex)]
preferredContentTokens pcd = concat
	[ preferredContentKeylessTokens pcd
	, preferredContentKeyedTokens pcd
	]

preferredContentParser :: [ParseToken (MatchFiles Annex)] -> String -> [ParseResult (MatchFiles Annex)]
preferredContentParser tokens = map (parseToken tokens) . tokenizeMatcher

mkLargeFilesParser :: Annex (String -> [ParseResult (MatchFiles Annex)])
mkLargeFilesParser = do
#ifdef WITH_MAGICMIME
	magicmime <- liftIO initMagicMime
	let mimer n f = ValueToken n (usev $ f magicmime)
#else
	let mimer n = ValueToken n $ 
		const $ Left $ "\""++n++"\" not supported; not built with MagicMime support"
#endif
	let parse = parseToken $
		commonKeyedTokens ++
		commonKeylessTokens LimitDiskFiles ++
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

call :: Either String (FileMatcher Annex) -> ParseResult (MatchFiles Annex)
call (Right sub) = Right $ Operation $ \notpresent mi ->
	matchMrun sub $ \a -> a notpresent mi
call (Left err) = Left err

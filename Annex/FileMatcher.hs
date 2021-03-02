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
	mkMatchExpressionParser,
	largeFilesMatcher,
	AddUnlockedMatcher,
	addUnlockedMatcher,
	checkAddUnlockedMatcher,
	LimitBy(..),
	module Types.FileMatcher
) where

import qualified Data.Map as M

import Annex.Common
import Limit
import Utility.Matcher
import Types.Group
import Types.FileMatcher
import Types.GitConfig
import Config.GitConfig
import Annex.SpecialRemote.Config (preferreddirField)
import Git.FilePath
import Types.Remote (RemoteConfig)
import Types.ProposedAccepted
import Annex.CheckAttr
import Git.CheckAttr (unspecifiedAttr)
import qualified Git.Config
#ifdef WITH_MAGICMIME
import Annex.Magic
#endif

import Data.Either
import qualified Data.Set as S

type GetFileMatcher = RawFilePath -> Annex (FileMatcher Annex)

checkFileMatcher :: GetFileMatcher -> RawFilePath -> Annex Bool
checkFileMatcher getmatcher file =
	checkFileMatcher' getmatcher file (return True)

-- | Allows running an action when no matcher is configured for the file.
checkFileMatcher' :: GetFileMatcher -> RawFilePath -> Annex Bool -> Annex Bool
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
		(_, AssociatedFile (Just file)) ->
			go =<< fileMatchInfo file mkey
		(Just key, AssociatedFile Nothing) ->
			let i = ProvidedInfo
				{ providedFilePath = Nothing
				, providedKey = Just key
				, providedFileSize = Nothing
				, providedMimeType = Nothing
				, providedMimeEncoding = Nothing
				, providedLinkType = Nothing
				}
			in go (MatchingInfo i)
		(Nothing, _) -> d
  where
	go mi = checkMatcher' matcher mi notpresent

checkMatcher' :: FileMatcher Annex -> MatchInfo -> AssumeNotPresent -> Annex Bool
checkMatcher' matcher mi notpresent =
	matchMrun matcher $ \o -> matchAction o notpresent mi

fileMatchInfo :: RawFilePath -> Maybe Key -> Annex MatchInfo
fileMatchInfo file mkey = do
	matchfile <- getTopFilePath <$> inRepo (toTopFilePath file)
	return $ MatchingFile FileInfo
		{ matchFile = matchfile
		, contentFile = file
		, matchKey = mkey
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
	preferreddir = maybe "public" fromProposedAccepted $
		M.lookup preferreddirField =<< (`M.lookup` configMap pcd) =<< repoUUID pcd

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

mkMatchExpressionParser :: Annex (String -> [ParseResult (MatchFiles Annex)])
mkMatchExpressionParser = do
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
			matchMagic "mimetype" getMagicMimeType providedMimeType userProvidedMimeType
		, mimer "mimeencoding" $
			matchMagic "mimeencoding" getMagicMimeEncoding providedMimeEncoding userProvidedMimeEncoding
		]
#else
		[ mimer "mimetype"
		, mimer "mimeencoding"
		]
#endif
	return $ map parse . tokenizeMatcher

{- Generates a matcher for files large enough (or meeting other criteria)
 - to be added to the annex, rather than directly to git.
 -
 - annex.largefiles is configured in git config, or git attributes,
 - or global git-annex config, in that order.
 -}
largeFilesMatcher :: Annex GetFileMatcher
largeFilesMatcher = go =<< getGitConfigVal' annexLargeFiles
  where
	go (HasGitConfig (Just expr)) = do
		matcher <- mkmatcher expr "git config"
		return $ const $ return matcher
	go v = return $ \file -> do
		expr <- checkAttr "annex.largefiles" file
		if null expr || expr == unspecifiedAttr
			then case v of
				HasGlobalConfig (Just expr') ->
					mkmatcher expr' "git-annex config"
				_ -> return matchAll
			else mkmatcher expr "gitattributes"

	mkmatcher expr cfgfrom = do
		parser <- mkMatchExpressionParser
		either (badexpr cfgfrom) return $ parsedToMatcher $ parser expr
	badexpr cfgfrom e = giveup $ "bad annex.largefiles configuration in " ++ cfgfrom ++ ": " ++ e

newtype AddUnlockedMatcher = AddUnlockedMatcher (FileMatcher Annex)

addUnlockedMatcher :: Annex AddUnlockedMatcher
addUnlockedMatcher = AddUnlockedMatcher <$> 
	(go =<< getGitConfigVal' annexAddUnlocked)
  where
	go (HasGitConfig (Just expr)) = mkmatcher expr "git config"
	go (HasGlobalConfig (Just expr)) = mkmatcher expr "git annex config"
	go _ = matchalways False

	mkmatcher :: String -> String -> Annex (FileMatcher Annex)
	mkmatcher expr cfgfrom = case Git.Config.isTrueFalse expr of
		Just b -> matchalways b
		Nothing -> do
			parser <- mkMatchExpressionParser
			either (badexpr cfgfrom) return $ parsedToMatcher $ parser expr
	badexpr cfgfrom e = giveup $ "bad annex.addunlocked configuration in " ++ cfgfrom ++ ": " ++ e

	matchalways True = return $ MOp limitAnything
	matchalways False = return $ MOp limitNothing

checkAddUnlockedMatcher :: AddUnlockedMatcher -> MatchInfo -> Annex Bool
checkAddUnlockedMatcher (AddUnlockedMatcher matcher) mi = 
	checkMatcher' matcher mi S.empty

simply :: MatchFiles Annex -> ParseResult (MatchFiles Annex)
simply = Right . Operation

usev :: MkLimit Annex -> String -> ParseResult (MatchFiles Annex)
usev a v = Operation <$> a v

call :: Either String (FileMatcher Annex) -> ParseResult (MatchFiles Annex)
call (Right sub) = Right $ Operation $ MatchFiles
	{ matchAction = \notpresent mi ->
		matchMrun sub $ \o -> matchAction o notpresent mi
	, matchNeedsFileName = any matchNeedsFileName sub
	, matchNeedsFileContent = any matchNeedsFileContent sub
	, matchNeedsKey = any matchNeedsKey sub
	, matchNeedsLocationLog = any matchNeedsLocationLog sub
	}
call (Left err) = Left err

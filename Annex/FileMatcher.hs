{- git-annex file matching
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.FileMatcher where

import qualified Data.Map as M

import Common.Annex
import Limit
import Utility.Matcher
import Types.Group
import Logs.Group
import Annex.UUID
import qualified Annex
import Git.FilePath

import Data.Either
import qualified Data.Set as S

type FileMatcher = Matcher MatchFiles

checkFileMatcher :: FileMatcher -> FilePath -> Annex Bool
checkFileMatcher matcher file = checkFileMatcher' matcher file S.empty True

checkFileMatcher' :: FileMatcher -> FilePath -> AssumeNotPresent -> Bool -> Annex Bool
checkFileMatcher' matcher file notpresent def
	| isEmpty matcher = return def
	| otherwise = do
		matchfile <- getTopFilePath <$> inRepo (toTopFilePath file)
		let fi = Annex.FileInfo
			{ Annex.matchFile = matchfile
			, Annex.relFile = file
			}
		matchMrun matcher $ \a -> a notpresent fi

matchAll :: FileMatcher
matchAll = generate []

parsedToMatcher :: [Either String (Token MatchFiles)] -> Either String FileMatcher
parsedToMatcher parsed = case partitionEithers parsed of
	([], vs) -> Right $ generate vs
	(es, _) -> Left $ unwords $ map ("Parse failure: " ++) es

parseToken :: MkLimit -> GroupMap -> String -> Either String (Token MatchFiles)
parseToken checkpresent groupmap t
	| t `elem` tokens = Right $ token t
	| t == "present" = use checkpresent
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
	use a = Operation <$> a v

{- This is really dumb tokenization; there's no support for quoted values.
 - Open and close parens are always treated as standalone tokens;
 - otherwise tokens must be separated by whitespace. -}
tokenizeMatcher :: String -> [String]
tokenizeMatcher = filter (not . null ) . concatMap splitparens . words
  where
	splitparens = segmentDelim (`elem` "()")

{- Generates a matcher for files large enough (or meeting other criteria)
 - to be added to the annex, rather than directly to git. -}
largeFilesMatcher :: Annex FileMatcher
largeFilesMatcher = go =<< annexLargeFiles <$> Annex.getGitConfig
  where
  	go Nothing = return matchAll
	go (Just expr) = do
		m <- groupMap
		u <- getUUID
		either badexpr return $ parsedToMatcher $
			map (parseToken (limitPresent $ Just u) m)
				(tokenizeMatcher expr)
	badexpr e = error $ "bad annex.largefiles configuration: " ++ e

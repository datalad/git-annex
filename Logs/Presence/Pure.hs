{- git-annex presence log, pure operations
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Presence.Pure where

import Annex.Common
import Annex.VectorClock
import Logs.Line
import Utility.QuickCheck

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.Attoparsec.ByteString.Char8 (char, anyChar)
import Data.ByteString.Builder

newtype LogInfo = LogInfo { fromLogInfo :: S.ByteString }
	deriving (Show, Eq, Ord)

data LogLine = LogLine
	{ date :: VectorClock
	, status :: LogStatus
	, info :: LogInfo
	} deriving (Eq, Show)

data LogStatus = InfoPresent | InfoMissing | InfoDead
	deriving (Eq, Show, Bounded, Enum)

parseLog :: L.ByteString -> [LogLine]
parseLog = fromMaybe [] . A.maybeResult . A.parse logParser

logParser :: A.Parser [LogLine]
logParser = parseLogLines $ LogLine
	<$> vectorClockParser
	<* char ' '
	<*> statusParser
	<* char ' '
	<*> (LogInfo <$> A.takeByteString)

statusParser :: A.Parser LogStatus
statusParser = do
	c <- anyChar
	case c of
		'1' -> return InfoPresent
		'0' -> return InfoMissing
		'X' -> return InfoDead
		_ -> fail "unknown status character"

parseStatus :: String -> Maybe LogStatus
parseStatus "1" = Just InfoPresent
parseStatus "0" = Just InfoMissing
parseStatus "X" = Just InfoDead
parseStatus _ = Nothing

buildLog :: [LogLine] -> Builder
buildLog = mconcat . map genline
  where
	genline (LogLine c s (LogInfo i)) = 
		buildVectorClock c <> sp <> genstatus s <> sp <> byteString i <> nl
	sp = charUtf8 ' '
	nl = charUtf8 '\n'
	genstatus InfoPresent = charUtf8 '1'
	genstatus InfoMissing = charUtf8 '0'
	genstatus InfoDead = charUtf8 'X'

{- Given a log, returns only the info that is are still in effect. -}
getLog :: L.ByteString -> [LogInfo]
getLog = map info . filterPresent . parseLog

{- Returns the info from LogLines that are in effect. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent = filter (\l -> InfoPresent == status l) . compactLog

{- Compacts a set of logs, returning a subset that contains the current
 - status. -}
compactLog :: [LogLine] -> [LogLine]
compactLog = mapLog . logMap

type LogMap = M.Map LogInfo LogLine

mapLog :: LogMap -> [LogLine]
mapLog = M.elems

logMap :: [LogLine] -> LogMap
logMap = foldr insertNewerLogLine M.empty

insertBetter :: (LogLine -> Bool) -> LogLine -> LogMap -> Maybe LogMap
insertBetter betterthan l m
	| better = Just (M.insert i l m)
	| otherwise = Nothing
  where
	better = maybe True betterthan (M.lookup i m)
	i = info l

{- Inserts a log into a map of logs, if the log has newer
 - information than the other logs in the map for the same info. -}
insertNewerLogLine :: LogLine -> LogMap -> LogMap
insertNewerLogLine l m = fromMaybe m $ insertBetter newer l m
  where
	newer l' = date l' <= date l

{- Inserts the log unless there's already one in the map with
 - the same status for its info, in which case there's no need to
 - change anything, to avoid log churn. -}
insertNewStatus :: LogLine -> LogMap -> Maybe LogMap
insertNewStatus l m  = insertBetter diffstatus l m
  where
	diffstatus l' = status l' /= status l

instance Arbitrary LogLine where
	arbitrary = LogLine
		<$> arbitrary
		<*> elements [minBound..maxBound]
		<*> (LogInfo <$> arbinfo)
	  where
		arbinfo = (encodeBS <$> arbitrary) `suchThat`
			(\b -> C8.notElem '\n' b && C8.notElem '\r' b)

prop_parse_build_presence_log :: [LogLine] -> Bool
prop_parse_build_presence_log l =
	parseLog (toLazyByteString (buildLog l)) == l

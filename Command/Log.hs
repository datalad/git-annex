{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Log where

import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import Data.Char

import Common.Annex
import Command
import qualified Logs.Location
import qualified Logs.Presence
import Annex.CatFile
import qualified Annex.Branch
import qualified Git
import Git.Command
import qualified Remote

def :: [Command]
def = [command "log" paramPaths seek "shows location log"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed $ start]

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = do
	showStart file ""
	showLog =<< readLog key
	stop

showLog :: [(POSIXTime, Git.Ref)] -> Annex ()
showLog ps = do
	zone <- liftIO getCurrentTimeZone
	sets <- mapM getset ps
	liftIO $ putStrLn ""
	mapM_ (diff zone) $ zip sets (drop 1 sets ++ genesis)
	where
		genesis = [(0, S.empty)]
		getset (ts, ref) = do
			s <- S.fromList <$> get ref
			return (ts, s)
		get ref = map toUUID . Logs.Presence.getLog . L.unpack <$>
			catObject ref
		diff zone ((ts, new), (_, old)) = do
			let time = show $ utcToLocalTime zone $
				posixSecondsToUTCTime ts
			output time True added
			output time False removed
			where
				added = S.difference new old
				removed = S.difference old new
		output time present s = do
			rs <- map (dropWhile isSpace) . lines <$>
				Remote.prettyPrintUUIDs "log" (S.toList s)
			liftIO $ mapM_ (putStrLn . indent . format) rs
				where
					format r = unwords
						[ time
						, if present then "+" else "-"
						, r
						]

getLog :: Key -> Annex [String]
getLog key = do
	top <- fromRepo Git.workTree
	p <- liftIO $ relPathCwdToFile top
	let logfile = p </> Logs.Location.logFile key
	inRepo $ pipeNullSplit
		[ Params "log -z --pretty=format:%ct --raw --abbrev=40"
		, Param $ show Annex.Branch.fullname
		, Param "--"
		, Param logfile
		]

readLog :: Key -> Annex [(POSIXTime, Git.Ref)]
readLog key = mapMaybe (parse . lines) <$> getLog key
	where
		parse (ts:raw:[]) = Just (parseTimeStamp ts, parseRaw raw)
		parse _ = Nothing

-- Parses something like ":100644 100644 oldsha newsha M"
parseRaw :: String -> Git.Ref
parseRaw l = Git.Ref $ words l !! 3

parseTimeStamp :: String -> POSIXTime
parseTimeStamp = utcTimeToPOSIXSeconds . fromMaybe (error "bad timestamp") .
	parseTime defaultTimeLocale "%s"

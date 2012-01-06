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
import qualified Option

def :: [Command]
def = [withOptions [afterOption, maxcountOption] $
	command "log" paramPaths seek "shows location log"]

afterOption :: Option
afterOption = Option.field [] "after" paramDate "show log after date"

maxcountOption :: Option
maxcountOption = Option.field ['n'] "max-count" paramNumber "limit number of logs displayed"

seek :: [CommandSeek]
seek = [withField afterOption return $ \afteropt ->
	withField maxcountOption return $ \maxcount ->
	withFilesInGit $ whenAnnexed $ start afteropt maxcount]

start :: Maybe String -> Maybe String -> FilePath -> (Key, Backend) -> CommandStart
start afteropt maxcount file (key, _) = do
	showLog file =<< (readLog <$> getLog key ps)
	stop
	where
		ps = concatMap (\(o, p) -> maybe [] p o)
			[ (afteropt, \d -> [Param "--after", Param d])
			, (maxcount, \c -> [Param "--max-count", Param c])
			]

showLog :: FilePath -> [(POSIXTime, (Git.Ref, Git.Ref))] -> Annex ()
showLog file ps = do
	zone <- liftIO getCurrentTimeZone
	sets <- mapM (getset snd) ps
	previous <- maybe (return genesis) (getset fst) (lastMaybe ps)
	mapM_ (diff zone) $ zip sets (drop 1 sets ++ [previous])
	where
		genesis = (0, S.empty)
		getset select (ts, refs) = do
			s <- S.fromList <$> get (select refs)
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
			liftIO $ mapM_ (putStrLn . format) rs
				where
					format r = unwords
						[ if present then "+" else "-"
						, time
						, file
						, "|"
						, r
						]

getLog :: Key -> [CommandParam] -> Annex [String]
getLog key ps = do
	top <- fromRepo Git.workTree
	p <- liftIO $ relPathCwdToFile top
	let logfile = p </> Logs.Location.logFile key
	inRepo $ pipeNullSplit $
		[ Params "log -z --pretty=format:%ct --raw --abbrev=40"
		, Param "--boundary"
		] ++ ps ++
		[ Param $ show Annex.Branch.fullname
		, Param "--"
		, Param logfile
		]

readLog :: [String] -> [(POSIXTime, (Git.Ref, Git.Ref))]
readLog = mapMaybe (parse . lines)
	where
		parse (ts:raw:[]) = Just (parseTimeStamp ts, parseRaw raw)
		parse _ = Nothing

-- Parses something like ":100644 100644 oldsha newsha M"
parseRaw :: String -> (Git.Ref, Git.Ref)
parseRaw l = (Git.Ref oldsha, Git.Ref newsha)
	where
		ws = words l
		oldsha = ws !! 2
		newsha = ws !! 3

parseTimeStamp :: String -> POSIXTime
parseTimeStamp = utcTimeToPOSIXSeconds . fromMaybe (error "bad timestamp") .
	parseTime defaultTimeLocale "%s"

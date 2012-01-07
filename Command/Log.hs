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
import qualified Annex

data RefChange = RefChange 
	{ changetime :: POSIXTime
	, oldref :: Git.Ref
	, newref :: Git.Ref
	}

def :: [Command]
def = [withOptions options $
	command "log" paramPaths seek "shows location log"]

options :: [Option]
options = 
	[ Option.field [] "since" paramDate "show log since date"
	, Option.field [] "after" paramDate "show log after date"
	, Option.field [] "until" paramDate "show log until date"
	, Option.field [] "before" paramDate "show log before date"
	, Option.field ['n'] "max-count" paramNumber "limit number of logs displayed"
	]

seek :: [CommandSeek]
seek = [withValue (concat <$> mapM getoption options) $ \os ->
	withFilesInGit $ whenAnnexed $ start os]
	where
		getoption o = maybe [] (use o) <$>
			Annex.getField (Option.name o)
		use o v = [Param ("--" ++ Option.name o), Param v]

start :: [CommandParam] -> FilePath -> (Key, Backend) -> CommandStart
start os file (key, _) = do
	showLog file =<< readLog <$> getLog key os
	stop

showLog :: FilePath -> [RefChange] -> Annex ()
showLog file ps = do
	zone <- liftIO getCurrentTimeZone
	sets <- mapM (getset newref) ps
	previous <- maybe (return genesis) (getset oldref) (lastMaybe ps)
	mapM_ (diff zone) $ zip sets (drop 1 sets ++ [previous])
	where
		genesis = (0, S.empty)
		getset select change = do
			s <- S.fromList <$> get (select change)
			return (changetime change, s)
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
					addel = if present then "+" else "-"
					format r = unwords
						[ addel, time, file, "|", r ]

getLog :: Key -> [CommandParam] -> Annex [String]
getLog key os = do
	top <- fromRepo Git.workTree
	p <- liftIO $ relPathCwdToFile top
	let logfile = p </> Logs.Location.logFile key
	inRepo $ pipeNullSplit $
		[ Params "log -z --pretty=format:%ct --raw --abbrev=40"
		] ++ os ++
		[ Param $ show Annex.Branch.fullname
		, Param "--"
		, Param logfile
		]

readLog :: [String] -> [RefChange]
readLog = mapMaybe (parse . lines)
	where
		parse (ts:raw:[]) = let (old, new) = parseRaw raw in
			Just RefChange
				{ changetime = parseTimeStamp ts
				, oldref = old
				, newref = new
				}
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

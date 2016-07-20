{- git-annex command
 -
 - Copyright 2012, 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Log where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Time.Clock.POSIX
import Data.Time
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif

import Command
import Logs
import Logs.Location
import qualified Annex.Branch
import qualified Git
import Git.Command
import qualified Remote
import qualified Annex

data RefChange = RefChange 
	{ changetime :: POSIXTime
	, oldref :: Git.Ref
	, newref :: Git.Ref
	, changekey :: Key
	}
	deriving (Show)

data LogChange = Added | Removed

type Outputter = LogChange -> POSIXTime -> [UUID] -> Annex ()

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "log" SectionQuery "shows location log"
		paramPaths (seek <$$> optParser)

data LogOptions = LogOptions
	{ logFiles :: CmdParams
	, allOption :: Bool
	, rawDateOption :: Bool
	, gourceOption :: Bool
	, passthruOptions :: [CommandParam]
	}

optParser :: CmdParamsDesc -> Parser LogOptions
optParser desc = LogOptions
	<$> cmdParams desc
	<*> switch
		( long "all"
		<> short 'A'
		<> help "display location log changes to all files"
		)
	<*> switch
		( long "raw-date"
		<> help "display seconds from unix epoch"
		)
	<*> switch
		( long "gource"
		<> help "format output for gource"
		)
	<*> (concat <$> many passthru)
  where
	passthru :: Parser [CommandParam]
	passthru = datepassthru "since"
		<|> datepassthru "after"
		<|> datepassthru "until"
		<|> datepassthru "before"
		<|> (mkpassthru "max-count" <$> strOption
			( long "max-count" <> metavar paramNumber
			<> help "limit number of logs displayed"
			))
	datepassthru n = mkpassthru n <$> strOption
		( long n <> metavar paramDate
		<> help ("show log " ++ n ++ " date")
		)
	mkpassthru n v = [Param ("--" ++ n), Param v]

seek :: LogOptions -> CommandSeek
seek o = do
	m <- Remote.uuidDescriptions
	zone <- liftIO getCurrentTimeZone
	let outputter = mkOutputter m zone o
	case (logFiles o, allOption o) of
		(fs, False) -> withFilesInGit (whenAnnexed $ start o outputter) fs
		([], True) -> commandAction (startAll o outputter)
		(_, True) -> error "Cannot specify both files and --all"

start :: LogOptions -> (FilePath -> Outputter) -> FilePath -> Key -> CommandStart
start o outputter file key = do
	(changes, cleanup) <- getKeyLog key (passthruOptions o)
	showLogIncremental (outputter file) changes
	void $ liftIO cleanup
	stop

startAll :: LogOptions -> (String -> Outputter) -> CommandStart
startAll o outputter = do
	(changes, cleanup) <- getAllLog (passthruOptions o)
	showLog outputter changes
	void $ liftIO cleanup
	stop

{- Displays changes made. Only works when all the RefChanges are for the
 - same key. The method is to compare each value with the value
 - after it in the list, which is the old version of the value.
 -
 - This ncessarily buffers the whole list, so does not stream.
 - But, the number of location log changes for a single key tends to be
 - fairly small.
 -
 - This minimizes the number of reads from git; each logged value is read
 - only once.
 -
 - This also generates subtly better output when the git-annex branch
 - got diverged.
 -}
showLogIncremental :: Outputter -> [RefChange] -> Annex ()
showLogIncremental outputter ps = do
	sets <- mapM (getset newref) ps
	previous <- maybe (return genesis) (getset oldref) (lastMaybe ps)
	let l = sets ++ [previous]
	let changes = map (\((t, new), (_, old)) -> (t, new, old))
		(zip l (drop 1 l))
	sequence_ $ compareChanges outputter changes
  where
	genesis = (0, S.empty)
	getset select change = do
		s <- S.fromList <$> loggedLocationsRef (select change)
		return (changetime change, s)

{- Displays changes made. Streams, and can display changes affecting
 - different keys, but does twice as much reading of logged values
 - as showLogIncremental. -}
showLog :: (String -> Outputter) -> [RefChange] -> Annex ()
showLog outputter cs = forM_ cs $ \c -> do
	let keyname = key2file (changekey c)
	new <- S.fromList <$> loggedLocationsRef (newref c)
	old <- S.fromList <$> loggedLocationsRef (oldref c)
	sequence_ $ compareChanges (outputter keyname)
		[(changetime c, new, old)]

mkOutputter :: M.Map UUID String -> TimeZone -> LogOptions -> FilePath -> Outputter
mkOutputter m zone o file
	| rawDateOption o = normalOutput lookupdescription file show
	| gourceOption o = gourceOutput lookupdescription file
	| otherwise = normalOutput lookupdescription file (showTimeStamp zone)
  where
	lookupdescription u = fromMaybe (fromUUID u) $ M.lookup u m

normalOutput :: (UUID -> String) -> FilePath -> (POSIXTime -> String) -> Outputter
normalOutput lookupdescription file formattime logchange ts us =
	liftIO $ mapM_ (putStrLn . format) us
  where
	time = formattime ts
	addel = case logchange of
		Added -> "+"
		Removed -> "-"
	format u = unwords [ addel, time, file, "|", 
		fromUUID u ++ " -- " ++ lookupdescription u ]

gourceOutput :: (UUID -> String) -> FilePath -> Outputter
gourceOutput lookupdescription file logchange ts us =
	liftIO $ mapM_ (putStrLn . intercalate "|" . format) us
  where
	time = takeWhile isDigit $ show ts
	addel = case logchange of
		Added -> "A" 
		Removed -> "M"
	format u = [ time, lookupdescription u, addel, file ]

{- Generates a display of the changes.
 - Uses a formatter to generate a display of items that are added and
 - removed. -}
compareChanges :: Ord a => (LogChange -> POSIXTime -> [a] -> b) -> [(POSIXTime, S.Set a, S.Set a)] -> [b]
compareChanges format changes = concatMap diff changes
  where
	diff (ts, new, old) =
		[ format Added ts   $ S.toList $ S.difference new old
		, format Removed ts $ S.toList $ S.difference old new
		]

{- Streams the git log for a given key's location log file.
 -
 - This is complicated by git log using paths relative to the current
 - directory, even when looking at files in a different branch. A wacky
 - relative path to the log file has to be used.
 -
 - The --remove-empty is a significant optimisation. It relies on location
 - log files never being deleted in normal operation. Letting git stop
 - once the location log file is gone avoids it checking all the way back
 - to commit 0 to see if it used to exist, so generally speeds things up a
 - *lot* for newish files. -}
getKeyLog :: Key -> [CommandParam] -> Annex ([RefChange], IO Bool)
getKeyLog key os = do
	top <- fromRepo Git.repoPath
	p <- liftIO $ relPathCwdToFile top
	config <- Annex.getGitConfig
	let logfile = p </> locationLogFile config key
	getGitLog [logfile] (Param "--remove-empty" : os)

{- Streams the git log for all git-annex branch changes. -}
getAllLog :: [CommandParam] -> Annex ([RefChange], IO Bool)
getAllLog = getGitLog []

getGitLog :: [FilePath] -> [CommandParam] -> Annex ([RefChange], IO Bool)
getGitLog fs os = do
	(ls, cleanup) <- inRepo $ pipeNullSplit $
		[ Param "log"
		, Param "-z"
		, Param "--pretty=format:%ct"
		, Param "--raw"
		, Param "--abbrev=40"
		] ++ os ++
		[ Param $ Git.fromRef Annex.Branch.fullname
		, Param "--"
		] ++ map Param fs
	return (parseGitRawLog ls, cleanup)

-- Parses chunked git log --raw output, which looks something like:
--
-- [ "timestamp\n:changeline"
-- , "logfile"
-- , ""
-- , "timestamp\n:changeline"
-- , "logfile"
-- , ":changeline"
-- , "logfile"
-- , ""
-- ]
--
-- The timestamp is not included before all changelines, so
-- keep track of the most recently seen timestamp.
parseGitRawLog :: [String] -> [RefChange]
parseGitRawLog = parse epoch
  where
	epoch = toEnum 0 :: POSIXTime
	parse oldts ([]:rest) = parse oldts rest
	parse oldts (c1:c2:rest) = case mrc of
		Just rc -> rc : parse ts rest
		Nothing -> parse ts (c2:rest)
	  where
		(ts, cl) = case separate (== '\n') c1 of
			(cl', []) -> (oldts, cl')
			(tss, cl') -> (parseTimeStamp tss, cl')
	  	mrc = do
			(old, new) <- parseRawChangeLine cl
			key <- locationLogFileKey c2
			return $ RefChange
				{ changetime = ts
				, oldref = old
				, newref = new
				, changekey = key
				}	
	parse _ _ = []

-- Parses something like "100644 100644 oldsha newsha M"
parseRawChangeLine :: String -> Maybe (Git.Ref, Git.Ref)
parseRawChangeLine = go . words
  where
	go (_:_:oldsha:newsha:_) = Just (Git.Ref oldsha, Git.Ref newsha)
	go _ = Nothing

parseTimeStamp :: String -> POSIXTime
parseTimeStamp = utcTimeToPOSIXSeconds . fromMaybe (error "bad timestamp") .
#if MIN_VERSION_time(1,5,0)
	parseTimeM True defaultTimeLocale "%s"
#else
	parseTime defaultTimeLocale "%s"
#endif

showTimeStamp :: TimeZone -> POSIXTime -> String
showTimeStamp zone = formatTime defaultTimeLocale rfc822DateFormat 
	. utcToZonedTime zone . posixSecondsToUTCTime

{- git-annex command
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Log where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Time.Clock.POSIX
import Data.Time
import qualified Data.ByteString.Char8 as B8
import qualified System.FilePath.ByteString as P

import Command
import Logs
import Logs.Location
import qualified Annex
import qualified Annex.Branch
import qualified Remote
import qualified Git
import Git.Log

data LogChange = Added | Removed

type Outputter = LogChange -> POSIXTime -> [UUID] -> Annex ()

cmd :: Command
cmd = withAnnexOptions [jsonOptions, annexedMatchingOptions] $
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
seek o = ifM (null <$> Annex.Branch.getUnmergedRefs)
	( do
		m <- Remote.uuidDescriptions
		zone <- liftIO getCurrentTimeZone
		outputter <- mkOutputter m zone o <$> jsonOutputEnabled
		let seeker = AnnexedFileSeeker
			{ startAction = start o outputter
			, checkContentPresent = Nothing
			-- the way this uses the location log would not be
			-- helped by precaching the current value
			, usesLocationLog = False
			}
		case (logFiles o, allOption o) of
			(fs, False) -> withFilesInGitAnnex ww seeker
				=<< workTreeItems ww fs
			([], True) -> commandAction (startAll o outputter)
			(_, True) -> giveup "Cannot specify both files and --all"
	, giveup "This repository is read-only, and there are unmerged git-annex branches, which prevents displaying location log changes. (Set annex.merge-annex-branches to false to ignore the unmerged git-annex branches.)"
	)
  where
	ww = WarnUnmatchLsFiles "log"

start :: LogOptions -> (ActionItem -> SeekInput -> Outputter) -> SeekInput -> RawFilePath -> Key -> CommandStart
start o outputter si file key = do
	(changes, cleanup) <- getKeyLog key (passthruOptions o)
	let ai = mkActionItem (file, key)
	showLogIncremental (outputter ai si) changes
	void $ liftIO cleanup
	stop

startAll :: LogOptions -> (ActionItem -> SeekInput -> Outputter) -> CommandStart
startAll o outputter = do
	(changes, cleanup) <- getGitLogAnnex [] (passthruOptions o)
	showLog (\ai -> outputter ai (SeekInput [])) changes
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
showLogIncremental :: Outputter -> [RefChange Key] -> Annex ()
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
showLog :: (ActionItem -> Outputter) -> [RefChange Key] -> Annex ()
showLog outputter cs = forM_ cs $ \c -> do
	let ai = mkActionItem (changed c)
	new <- S.fromList <$> loggedLocationsRef (newref c)
	old <- S.fromList <$> loggedLocationsRef (oldref c)
	sequence_ $ compareChanges (outputter ai)
		[(changetime c, new, old)]

mkOutputter :: UUIDDescMap -> TimeZone -> LogOptions -> Bool -> ActionItem -> SeekInput -> Outputter
mkOutputter m zone o jsonenabled ai si
	| jsonenabled = jsonOutput m ai si
	| rawDateOption o = normalOutput lookupdescription ai rawTimeStamp
	| gourceOption o = gourceOutput lookupdescription ai 
	| otherwise = normalOutput lookupdescription ai (showTimeStamp zone)
  where
	lookupdescription u = maybe (fromUUID u) (fromUUIDDesc) (M.lookup u m)

normalOutput :: (UUID -> String) -> ActionItem -> (POSIXTime -> String) -> Outputter
normalOutput lookupdescription ai formattime logchange ts us = do
	qp <- coreQuotePath <$> Annex.getGitConfig
	liftIO $ mapM_ (B8.putStrLn . quote qp . format) us
  where
	time = formattime ts
	addel = case logchange of
		Added -> "+"
		Removed -> "-"
	format u = UnquotedString addel <> " " 
		<> UnquotedString time <> " " 
		<> actionItemDesc ai <> " | " 
		<> UnquotedByteString (fromUUID u) <> " -- "
		<> UnquotedString (lookupdescription u)

jsonOutput :: UUIDDescMap -> ActionItem -> SeekInput -> Outputter
jsonOutput m ai si logchange ts us = do
	showStartMessage $ StartMessage "log" ai si
	maybeShowJSON $ JSONChunk
		[ ("logged", case logchange of
			Added -> "addition"
			Removed -> "removal")
		, ("date", rawTimeStamp ts)
		]
	void $ Remote.prettyPrintUUIDsDescs "locations" m us
	showEndOk

gourceOutput :: (UUID -> String) -> ActionItem -> Outputter
gourceOutput lookupdescription ai logchange ts us =
	liftIO $ mapM_ (putStrLn . intercalate "|" . format) us
  where
	time = takeWhile isDigit $ show ts
	addel = case logchange of
		Added -> "A" 
		Removed -> "M"
	format u =
		[ time
		, lookupdescription u
		, addel
		, decodeBS (noquote (actionItemDesc ai))
		]

{- Generates a display of the changes.
 - Uses a formatter to generate a display of items that are added and
 - removed. -}
compareChanges :: Ord a => (LogChange -> POSIXTime -> [a] -> b) -> [(POSIXTime, S.Set a, S.Set a)] -> [b]
compareChanges format changes = concatMap diff changes
  where
	diff (ts, new, old)
		| new == old = []
		| otherwise = 
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
getKeyLog :: Key -> [CommandParam] -> Annex ([RefChange Key], IO Bool)
getKeyLog key os = do
	top <- fromRepo Git.repoPath
	p <- liftIO $ relPathCwdToFile top
	config <- Annex.getGitConfig
	let logfile = p P.</> locationLogFile config key
	getGitLogAnnex [fromRawFilePath logfile] (Param "--remove-empty" : os)

getGitLogAnnex :: [FilePath] -> [CommandParam] -> Annex ([RefChange Key], IO Bool)
getGitLogAnnex fs os = do
	config <- Annex.getGitConfig
	let fileselector = locationLogFileKey config . toRawFilePath
	inRepo $ getGitLog Annex.Branch.fullname fs os fileselector

showTimeStamp :: TimeZone -> POSIXTime -> String
showTimeStamp zone = formatTime defaultTimeLocale rfc822DateFormat 
	. utcToZonedTime zone . posixSecondsToUTCTime

rawTimeStamp :: POSIXTime -> String
rawTimeStamp t = filter (/= 's') (show t)

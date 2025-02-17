{- git-annex command
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Command.Log where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Time.Clock.POSIX
import Data.Time
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.Async

import Command
import Logs
import Logs.Location
import Logs.UUID
import qualified Logs.Presence.Pure as PLog
import Logs.Trust.Pure (parseTrustLog)
import Logs.UUIDBased (simpleMap)
import qualified Annex
import qualified Annex.Branch
import qualified Remote
import qualified Git
import Git.Log
import Git.CatFile
import Types.TrustLevel
import Utility.DataUnits
import Utility.HumanTime
import qualified Utility.FileIO as F

data LogChange = Added | Removed

type Outputter = LogChange -> POSIXTime -> [UUID] -> Annex ()

cmd :: Command
cmd = withAnnexOptions [jsonOptions, annexedMatchingOptions] $
	command "log" SectionQuery "shows location log"
		paramPaths (seek <$$> optParser)

data LogOptions = LogOptions
	{ logFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, sizesOfOption :: Maybe (DeferredParse UUID)
	, sizesOption :: Bool
	, totalSizesOption :: Bool
	, intervalOption :: Maybe Duration
	, receivedOption :: Bool
	, gnuplotOption :: Bool
	, rawDateOption :: Bool
	, bytesOption :: Bool
	, gourceOption :: Bool
	, passthruOptions :: [CommandParam]
	}

optParser :: CmdParamsDesc -> Parser LogOptions
optParser desc = LogOptions
	<$> cmdParams desc
	<*> optional parseKeyOptions
	<*> optional ((parseUUIDOption <$> strOption
		( long "sizesof"
		<> metavar (paramRemote `paramOr` paramDesc `paramOr` paramUUID)
		<> help "display history of sizes of this repository"
		<> completeRemotes
		)))
	<*> switch
		( long "sizes"
		<> help "display history of sizes of all repositories"
		)
	<*> switch
		( long "totalsizes"
		<> help "display history of total sizes of all repositories"
		)
	<*> optional (option (eitherReader parseDuration)
		( long "interval" <> metavar paramTime
		<> help "minimum time between displays of changed size"
		))
	<*> switch
		( long "received"
		<> help "display received data per interval rather than repository sizes"
		)
	<*> switch
		( long "gnuplot"
		<> help "graph the history"
		)
	<*> switch
		( long "raw-date"
		<> help "display seconds from unix epoch"
		)
	<*> switch
		( long "bytes"
		<> help "display sizes in bytes"
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
	( maybe (pure Nothing) (Just <$$> getParsed) (sizesOfOption o) >>= \case
		Just u -> sizeHistoryInfo (Just u) o
		Nothing -> if sizesOption o || totalSizesOption o
			then sizeHistoryInfo Nothing o
			else go	
	, giveup "This repository is read-only, and there are unmerged git-annex branches, which prevents displaying location log changes. (Set annex.merge-annex-branches to false to ignore the unmerged git-annex branches.)"
	)
  where
	ww = WarnUnmatchLsFiles "log"
	go = do
		m <- Remote.uuidDescriptions
		zone <- liftIO getCurrentTimeZone
		outputter <- mkOutputter m zone o <$> jsonOutputEnabled
		let seeker = AnnexedFileSeeker
			{ startAction = const $ \si file key ->
				start o outputter (si, key, mkActionItem (file, key))
			, checkContentPresent = Nothing
			-- the way this uses the location log would not be
			-- helped by precaching the current value
			, usesLocationLog = False
			}
		case (logFiles o, keyOptions o) of
			([], Just WantAllKeys) -> 
				commandAction (startAll o outputter)
			(fs, ko) -> withKeyOptions ko False
				seeker (commandAction . start o outputter)
				(withFilesInGitAnnex ww seeker)
				=<< workTreeItems ww fs

start :: LogOptions -> (ActionItem -> SeekInput -> Outputter) -> (SeekInput, Key, ActionItem) -> CommandStart
start o outputter (si, key, ai) = do
	(changes, cleanup) <- getKeyLog key (passthruOptions o)
	showLogIncremental (outputter ai si) changes
	void $ liftIO cleanup
	stop

startAll :: LogOptions -> (ActionItem -> SeekInput -> Outputter) -> CommandStart
startAll o outputter = do
	(changes, cleanup) <- getGitLogAnnex [] (passthruOptions o)
	showLog (\ai -> outputter ai (SeekInput [])) changes
	void $ liftIO cleanup
	stop

{- Displays changes made. Only works when all the LoggedFileChanges are for the
 - same key. The method is to compare each value with the value
 - after it in the list, which is the old version of the value.
 -
 - This necessarily buffers the whole list, so does not stream.
 - But, the number of location log changes for a single key tends to be
 - fairly small.
 -
 - This minimizes the number of reads from git; each logged value is read
 - only once.
 -
 - This also generates subtly better output when the git-annex branch
 - got diverged.
 -}
showLogIncremental :: Outputter -> [LoggedFileChange Key] -> Annex ()
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
showLog :: (ActionItem -> Outputter) -> [LoggedFileChange Key] -> Annex ()
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
	| otherwise = normalOutput lookupdescription ai (showTimeStamp zone rfc822DateFormat)
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
getKeyLog :: Key -> [CommandParam] -> Annex ([LoggedFileChange Key], IO Bool)
getKeyLog key os = do
	top <- fromRepo Git.repoPath
	p <- liftIO $ relPathCwdToFile top
	config <- Annex.getGitConfig
	let logfile = p </> locationLogFile config key
	getGitLogAnnex [logfile] (Param "--remove-empty" : os)

getGitLogAnnex :: [OsPath] -> [CommandParam] -> Annex ([LoggedFileChange Key], IO Bool)
getGitLogAnnex fs os = do
	config <- Annex.getGitConfig
	let fileselector = \_sha f ->
		locationLogFileKey config f
	inRepo $ getGitLog Annex.Branch.fullname Nothing (map fromOsPath fs) os fileselector

showTimeStamp :: TimeZone -> String -> POSIXTime -> String
showTimeStamp zone format = formatTime defaultTimeLocale format
	. utcToZonedTime zone . posixSecondsToUTCTime

rawTimeStamp :: POSIXTime -> String
rawTimeStamp t = filter (/= 's') (show t)

sizeHistoryInfo :: (Maybe UUID) -> LogOptions -> Annex ()
sizeHistoryInfo mu o = do
	uuidmap <- getuuidmap
	zone <- liftIO getCurrentTimeZone
	dispst <- displaystart uuidmap zone
	(l, cleanup) <- getlog
	g <- Annex.gitRepo
	liftIO $ catObjectStream g $ \feeder closer reader -> do
		tid <- async $ do
			forM_ l $ \c -> 
				feeder ((changed c, changetime c), newref c)
			closer
		go reader mempty mempty mempty uuidmap dispst
		wait tid
	void $ liftIO cleanup
  where
	-- Go through the log of the git-annex branch in reverse,
	-- and in date order, and pick out changes to location log files
	-- and to the trust log.
	getlog = do
		config <- Annex.getGitConfig
		let fileselector = \_sha f ->
			case locationLogFileKey config f of
				Just k -> Just (Right k)
				Nothing
					| f == trustLog -> Just (Left ())
					| otherwise -> Nothing
		inRepo $ getGitLog Annex.Branch.fullname Nothing []
			[ Param "--date-order"
			, Param "--reverse"
			]
			fileselector

	go reader sizemap locmap trustlog uuidmap dispst = reader >>= \case
		Just ((Right k, t), Just logcontent) -> do
			let !newlog = parselocationlog logcontent uuidmap
			let !(sizemap', locmap') = case M.lookup k locmap of
				Nothing -> addnew k sizemap locmap newlog
				Just v -> update k sizemap locmap v newlog
			dispst' <- displaysizes dispst trustlog uuidmap sizemap' t
			go reader sizemap' locmap' trustlog uuidmap dispst'
		Just ((Left (), t), Just logcontent) -> do
			let !trustlog' = trustlog <> parseTrustLog logcontent
			dispst' <- displaysizes dispst trustlog' uuidmap sizemap t
			go reader sizemap locmap trustlog' uuidmap dispst'
		Just (_, Nothing) -> 
			go reader sizemap locmap trustlog uuidmap dispst
		Nothing -> 
			displayend dispst

	-- Known uuids are stored in this map, and when uuids are stored in the
	-- state, it's a value from this map. This avoids storing multiple
	-- copies of the same uuid in memory.
	getuuidmap = do
		(us, ds) <- unzip . M.toList <$> uuidDescMap
		return $ M.fromList (zip us (zip us ds))
	
	-- Parses a location log file, and replaces the logged uuid
	-- with one from the uuidmap.
	parselocationlog logcontent uuidmap = 
		map replaceuuid $ PLog.parseLog logcontent
	  where
		replaceuuid ll = 
			let !u = toUUID $ PLog.fromLogInfo $ PLog.info ll
			    !ushared = maybe u fst $ M.lookup u uuidmap
			in ll { PLog.info = PLog.LogInfo (fromUUID ushared) }

	presentlocs = map (toUUID . PLog.fromLogInfo . PLog.info)
		. PLog.filterPresent
	
	-- Since the git log is being traversed in date order, commits
	-- from different branches can appear one after the other, and so
	-- the newlog is not necessarily the complete state known at that
	-- time across all git-annex repositories.
	--
	-- This combines the new location log with what has been
	-- accumulated so far, which is equivalent to merging together
	-- all git-annex branches at that point in time.
	update k sizemap locmap (oldlog, oldlocs) newlog = 
		( updatesize (updatesize sizemap sz (S.toList addedlocs))
			(negate sz) (S.toList removedlocs)
		, M.insert k (combinedlog, combinedlocs) locmap
		)
	  where
		sz = ksz k
		combinedlog = PLog.compactLog (oldlog ++ newlog)
		combinedlocs = S.fromList (presentlocs combinedlog)
		addedlocs = S.difference combinedlocs oldlocs
		removedlocs
			| receivedOption o = S.empty
			| otherwise = S.difference oldlocs combinedlocs
	
	addnew k sizemap locmap newlog = 
		( updatesize sizemap (ksz k) locs
		, M.insert k (newlog, S.fromList locs) locmap
		)
	  where
		locs = presentlocs newlog
	
	ksz k = fromMaybe 0 (fromKey keySize k)
	
	updatesize sizemap _ [] = sizemap
	updatesize sizemap sz (l:ls) =
		updatesize (M.insertWith (+) l sz sizemap) sz ls

	epoch = toEnum 0

	displaystart uuidmap zone
		| gnuplotOption o = do
			file <- (</>)
				<$> fromRepo gitAnnexDir
				<*> pure (literalOsPath "gnuplot")
			liftIO $ putStrLn $ "Generating gnuplot script in " ++ fromOsPath file
			h <- liftIO $ F.openFile file WriteMode
			liftIO $ mapM_ (hPutStrLn h)
				[ "set datafile separator ','"
				, "set timefmt \"%Y-%m-%dT%H:%M:%S\""
				, "set xdata time"
				, "set xtics out"
				, "set ytics format '%s%c'"
				, "set tics front"
				, "set key spacing 1 font \",8\""
				]
			unless (sizesOption o) $
				liftIO $ hPutStrLn h "set key off"
			liftIO $ hPutStrLn h "$data << EOD"
			liftIO $ hPutStrLn h $ if sizesOption o
				then uuidmapheader
				else csvheader ["value"]
			let endaction = do
				mapM_ (hPutStrLn h)
					[ "EOD"
					, ""
					, "plot for [i=2:" ++ show ncols ++ ":1] \\"
					, "  \"$data\" using 1:(sum [col=i:" ++ show ncols ++ "] column(col)) \\"
					, "  title columnheader(i) \\"
					, if receivedOption o
						then "  with boxes"
						else "  with filledcurves x1"
					]
				hFlush h
				putStrLn $ "Running gnuplot..."
				void $ liftIO $ boolSystem "gnuplot"
					[Param "-p", File (fromOsPath file)]
			return (dispst h endaction)
		| sizesOption o = do
			liftIO $ putStrLn uuidmapheader
			return (dispst stdout noop)
		| otherwise = return (dispst stdout noop)
	  where
		dispst fileh endaction = 
			(zone, False, epoch, Nothing, mempty, fileh, endaction)
		ncols
			| sizesOption o = 1 + length (M.elems uuidmap)
			| otherwise = 2
		uuidmapheader = csvheader $
			map (fromUUIDDesc . snd) (M.elems uuidmap)

	displaysizes (zone, displayedyet, prevt, prevoutput, prevsizemap, h, endaction) trustlog uuidmap sizemap t
		| t - prevt >= dt && changedoutput = do
			displayts zone t output h
			return (zone, True, t, Just output, sizemap', h, endaction)
		| t < prevt = return (zone, displayedyet, t, Just output, prevsizemap, h, endaction)
		| otherwise = return (zone, displayedyet, prevt, prevoutput, prevsizemap, h, endaction)
	  where
		output = intercalate "," (map showsize sizes)
		us = case mu of
			Just u -> [u]
			Nothing -> M.keys uuidmap
		sizes
			| totalSizesOption o = [sum (M.elems sizedisplaymap)]
			| otherwise = map (\u -> fromMaybe 0 (M.lookup u sizedisplaymap)) us
		dt = maybe 1 durationToPOSIXTime (intervalOption o)

		changedoutput
			| receivedOption o = 
				any (/= 0) sizes 
					|| prevoutput /= Just output
			| otherwise = 
				(displayedyet || any (/= 0) sizes)
					&& (prevoutput /= Just output)

		sizedisplaymap
			| receivedOption o = 
				M.unionWith posminus sizemap' prevsizemap
			| otherwise = sizemap'

		posminus a b = max 0 (a - b)

		-- A version of sizemap where uuids that are currently dead
		-- have 0 size.
		sizemap' = M.mapWithKey zerodead sizemap
		zerodead u v = case M.lookup u (simpleMap trustlog) of
			Just DeadTrusted -> 0
			_ -> v

	displayts zone t output h = do
		hPutStrLn h (ts ++ "," ++ output)
		hFlush h
	  where
		ts = if rawDateOption o && not (gnuplotOption o)
			then rawTimeStamp t
			else showTimeStamp zone "%Y-%m-%dT%H:%M:%S" t

	displayend dispst@(_, _, _, _, _, _, endaction) = do
		displayendsizes dispst
		endaction

	displayendsizes (zone, _, _, Just output, _, h, _) = do
		now <- getPOSIXTime
		displayts zone now output h
	displayendsizes _ = return ()

	showsize n
		| bytesOption o || gnuplotOption o = show n
		| otherwise = roughSize storageUnits True n
	
	csvquote s
		| ',' `elem` s || '"' `elem` s = 
			'"' : concatMap escquote s ++ ['"']
		| otherwise = s
	  where
		escquote '"' = "\"\""
		escquote c = [c]
	
	csvheader l = intercalate "," ("date" : map csvquote l)

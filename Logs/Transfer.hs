{- git-annex transfer information files and lock files
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Logs.Transfer where

import Types.Transfer
import Types.ActionItem
import Annex.Common
import qualified Git
import Utility.Metered
import Utility.Percentage
import Utility.PID
import Annex.LockPool
import Utility.TimeStamp
import Logs.File
#ifndef mingw32_HOST_OS
import Annex.Perms
#endif

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Concurrent

describeTransfer :: Transfer -> TransferInfo -> String
describeTransfer t info = unwords
	[ show $ transferDirection t
	, show $ transferUUID t
	, decodeBS' $ actionItemDesc $ ActionItemAssociatedFile
		(associatedFile info)
		(transferKey t)
	, show $ bytesComplete info
	]

{- Transfers that will accomplish the same task. -}
equivilantTransfer :: Transfer -> Transfer -> Bool
equivilantTransfer t1 t2
	| transferDirection t1 == Download && transferDirection t2 == Download &&
	  transferKeyData t1 == transferKeyData t2 = True
	| otherwise = t1 == t2

percentComplete :: Transfer -> TransferInfo -> Maybe Percentage
percentComplete t info =
	percentage
		<$> keySize (transferKeyData t)
		<*> Just (fromMaybe 0 $ bytesComplete info)

{- Generates a callback that can be called as transfer progresses to update
 - the transfer info file. Also returns the file it'll be updating, 
 - an action that sets up the file with appropriate permissions,
 - which should be run after locking the transfer lock file, but
 - before using the callback, and a MVar that can be used to read
 - the number of bytesComplete. -}
mkProgressUpdater :: Transfer -> TransferInfo -> Annex (MeterUpdate, FilePath, Annex (), MVar Integer)
mkProgressUpdater t info = do
	tfile <- fromRepo $ transferFile t
	let createtfile = void $ tryNonAsync $ writeTransferInfoFile info tfile
	mvar <- liftIO $ newMVar 0
	return (liftIO . updater tfile mvar, tfile, createtfile, mvar)
  where
	updater tfile mvar b = modifyMVar_ mvar $ \oldbytes -> do
		let newbytes = fromBytesProcessed b
		if newbytes - oldbytes >= mindelta
			then do
				let info' = info { bytesComplete = Just newbytes }
				_ <- tryIO $ updateTransferInfoFile info' tfile
				return newbytes
			else return oldbytes
	{- The minimum change in bytesComplete that is worth
	 - updating a transfer info file for is 1% of the total
	 - keySize, rounded down. -}
	mindelta = case keySize (transferKeyData t) of
		Just sz -> sz `div` 100
		Nothing -> 100 * 1024 -- arbitrarily, 100 kb

startTransferInfo :: AssociatedFile -> IO TransferInfo
startTransferInfo afile = TransferInfo
	<$> (Just . utcTimeToPOSIXSeconds <$> getCurrentTime)
#ifndef mingw32_HOST_OS
	<*> pure Nothing -- pid not stored in file, so omitted for speed
#else
	<*> (Just <$> getPID)
#endif
	<*> pure Nothing -- tid ditto
	<*> pure Nothing -- not 0; transfer may be resuming
	<*> pure Nothing
	<*> pure afile
	<*> pure False

{- If a transfer is still running, returns its TransferInfo.
 - 
 - If no transfer is running, attempts to clean up the stale
 - lock and info files. This can happen if a transfer process was
 - interrupted.
 -}
checkTransfer :: Transfer -> Annex (Maybe TransferInfo)
checkTransfer t = debugLocks $ do
	tfile <- fromRepo $ transferFile t
	let lck = transferLockFile tfile
	let cleanstale = do
		void $ tryIO $ removeFile tfile
		void $ tryIO $ removeFile lck
#ifndef mingw32_HOST_OS
	v <- getLockStatus lck
	case v of
		StatusLockedBy pid -> liftIO $ catchDefaultIO Nothing $
			readTransferInfoFile (Just pid) tfile
		_ -> do
			-- Take a non-blocking lock while deleting
			-- the stale lock file. Ignore failure
			-- due to permissions problems, races, etc.
			void $ tryIO $ do
				mode <- annexFileMode
				r <- tryLockExclusive (Just mode) lck
				case r of
					Just lockhandle -> liftIO $ do
						cleanstale
						dropLock lockhandle
					_ -> noop
			return Nothing
#else
	v <- liftIO $ lockShared lck
	liftIO $ case v of
		Nothing -> catchDefaultIO Nothing $
			readTransferInfoFile Nothing tfile
		Just lockhandle -> do
			dropLock lockhandle
			cleanstale
			return Nothing
#endif

{- Gets all currently running transfers. -}
getTransfers :: Annex [(Transfer, TransferInfo)]
getTransfers = getTransfers' [Download, Upload] (const True)

getTransfers' :: [Direction] -> (Key -> Bool) -> Annex [(Transfer, TransferInfo)]
getTransfers' dirs wanted = do
	transfers <- filter (wanted . transferKey)
		<$> mapMaybe parseTransferFile . concat <$> findfiles
	infos <- mapM checkTransfer transfers
	return $ mapMaybe running $ zip transfers infos
  where
	findfiles = liftIO . mapM dirContentsRecursive
		=<< mapM (fromRepo . transferDir) dirs
	running (t, Just i) = Just (t, i)
	running (_, Nothing) = Nothing

{- Number of bytes remaining to download from matching downloads that are in
 - progress. -}
sizeOfDownloadsInProgress :: (Key -> Bool) -> Annex Integer
sizeOfDownloadsInProgress wanted = sum . map remaining
	<$> getTransfers' [Download] wanted
  where
	remaining (t, info) =
		case (fromKey keySize (transferKey t), bytesComplete info) of
			(Just sz, Just done) -> sz - done
			(Just sz, Nothing) -> sz
			(Nothing, _) -> 0

{- Gets failed transfers for a given remote UUID. -}
getFailedTransfers :: UUID -> Annex [(Transfer, TransferInfo)]
getFailedTransfers u = catMaybes <$> (liftIO . getpairs =<< concat <$> findfiles)
  where
	getpairs = mapM $ \f -> do
		let mt = parseTransferFile f
		mi <- readTransferInfoFile Nothing f
		return $ case (mt, mi) of
			(Just t, Just i) -> Just (t, i)
			_ -> Nothing
	findfiles = liftIO . mapM dirContentsRecursive
		=<< mapM (fromRepo . failedTransferDir u) [Download, Upload]

clearFailedTransfers :: UUID -> Annex [(Transfer, TransferInfo)]
clearFailedTransfers u = do
	failed <- getFailedTransfers u
	mapM_ (removeFailedTransfer . fst) failed
	return failed

removeFailedTransfer :: Transfer -> Annex ()
removeFailedTransfer t = do
	f <- fromRepo $ failedTransferFile t
	liftIO $ void $ tryIO $ removeFile f

recordFailedTransfer :: Transfer -> TransferInfo -> Annex ()
recordFailedTransfer t info = do
	failedtfile <- fromRepo $ failedTransferFile t
	writeTransferInfoFile info failedtfile

{- The transfer information file to use for a given Transfer. -}
transferFile :: Transfer -> Git.Repo -> FilePath
transferFile (Transfer direction u kd) r = transferDir direction r
	</> filter (/= '/') (fromUUID u)
	</> fromRawFilePath (keyFile (mkKey (const kd)))

{- The transfer information file to use to record a failed Transfer -}
failedTransferFile :: Transfer -> Git.Repo -> FilePath
failedTransferFile (Transfer direction u kd) r = failedTransferDir u direction r
	</> fromRawFilePath (keyFile (mkKey (const kd)))

{- The transfer lock file corresponding to a given transfer info file. -}
transferLockFile :: FilePath -> FilePath
transferLockFile infofile = let (d,f) = splitFileName infofile in
	combine d ("lck." ++ f)

{- Parses a transfer information filename to a Transfer. -}
parseTransferFile :: FilePath -> Maybe Transfer
parseTransferFile file
	| "lck." `isPrefixOf` takeFileName file = Nothing
	| otherwise = case drop (length bits - 3) bits of
		[direction, u, key] -> Transfer
			<$> parseDirection direction
			<*> pure (toUUID u)
			<*> fmap (fromKey id) (fileKey (toRawFilePath key))
		_ -> Nothing
  where
	bits = splitDirectories file

writeTransferInfoFile :: TransferInfo -> FilePath -> Annex ()
writeTransferInfoFile info tfile = writeLogFile tfile $ writeTransferInfo info

-- The file keeps whatever permissions it has, so should be used only
-- after it's been created with the right perms by writeTransferInfoFile.
updateTransferInfoFile :: TransferInfo -> FilePath -> IO ()
updateTransferInfoFile info tfile = writeFile tfile $ writeTransferInfo info

{- File format is a header line containing the startedTime and any
 - bytesComplete value. Followed by a newline and the associatedFile.
 -
 - On unix, the transferPid is not included; instead it is obtained
 - by looking at the process that locks the file.
 -
 - On windows, the transferPid is included, as a second line.
 -}
writeTransferInfo :: TransferInfo -> String
writeTransferInfo info = unlines
	[ (maybe "" show $ startedTime info) ++
	  (maybe "" (\b -> ' ' : show b) (bytesComplete info))
#ifdef mingw32_HOST_OS
	, maybe "" show (transferPid info)
#endif
	-- comes last; arbitrary content
	, let AssociatedFile afile = associatedFile info
	  in maybe "" fromRawFilePath afile
	]

readTransferInfoFile :: Maybe PID -> FilePath -> IO (Maybe TransferInfo)
readTransferInfoFile mpid tfile = catchDefaultIO Nothing $
	readTransferInfo mpid <$> readFileStrict tfile

readTransferInfo :: Maybe PID -> String -> Maybe TransferInfo
readTransferInfo mpid s = TransferInfo
	<$> time
#ifdef mingw32_HOST_OS
	<*> pure (if isJust mpid then mpid else mpid')
#else
	<*> pure mpid
#endif
	<*> pure Nothing
	<*> pure Nothing
	<*> bytes
	<*> pure (AssociatedFile (if null filename then Nothing else Just (toRawFilePath filename)))
	<*> pure False
  where
#ifdef mingw32_HOST_OS
	(firstline, otherlines) = separate (== '\n') s
	(secondline, rest) = separate (== '\n') otherlines
	mpid' = readish secondline
#else
	(firstline, rest) = separate (== '\n') s
#endif
	filename
		| end rest == "\n" = beginning rest
		| otherwise = rest
	bits = splitc ' ' firstline
	numbits = length bits
	time = if numbits > 0
		then Just <$> parsePOSIXTime =<< headMaybe bits
		else pure Nothing -- not failure
	bytes = if numbits > 1
		then Just <$> readish =<< headMaybe (drop 1 bits)
		else pure Nothing -- not failure

{- The directory holding transfer information files for a given Direction. -}
transferDir :: Direction -> Git.Repo -> FilePath
transferDir direction r = gitAnnexTransferDir r </> formatDirection direction

{- The directory holding failed transfer information files for a given
 - Direction and UUID -}
failedTransferDir :: UUID -> Direction -> Git.Repo -> FilePath
failedTransferDir u direction r = gitAnnexTransferDir r
	</> "failed"
	</> formatDirection direction
	</> filter (/= '/') (fromUUID u)

prop_read_write_transferinfo :: TransferInfo -> Bool
prop_read_write_transferinfo info
	| isJust (transferRemote info) = True -- remote not stored
	| isJust (transferTid info) = True -- tid not stored
	| otherwise = Just (info { transferPaused = False }) == info'
  where
	info' = readTransferInfo (transferPid info) (writeTransferInfo info)


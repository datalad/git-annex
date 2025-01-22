{- git-annex transfer information files and lock files
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Logs.Transfer where

import Types.Transfer
import Types.ActionItem
import Annex.Common
import qualified Git
import qualified Git.Quote
import Utility.Metered
import Utility.Percentage
import Utility.PID
import Annex.LockPool
import Utility.TimeStamp
import Logs.File
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F
#ifndef mingw32_HOST_OS
import Annex.Perms
#endif

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified System.FilePath.ByteString as P

describeTransfer :: Git.Quote.QuotePath -> Transfer -> TransferInfo -> String
describeTransfer qp t info = unwords
	[ show $ transferDirection t
	, show $ transferUUID t
	, decodeBS $ quote qp $ actionItemDesc $ ActionItemAssociatedFile
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
 - the transfer info file. Also returns an action that sets up the file with
 - appropriate permissions, which should be run after locking the transfer
 - lock file, but before using the callback, and a TVar that can be used to
 - read the number of bytes processed so far. -}
mkProgressUpdater :: Transfer -> TransferInfo -> RawFilePath -> Annex (MeterUpdate, Annex (), TVar (Maybe BytesProcessed))
mkProgressUpdater t info tfile = do
	let createtfile = void $ tryNonAsync $ writeTransferInfoFile info tfile
	tvar <- liftIO $ newTVarIO Nothing
	loggedtvar <- liftIO $ newTVarIO 0
	return (liftIO . updater (fromRawFilePath tfile) tvar loggedtvar, createtfile, tvar)
  where
	updater tfile' tvar loggedtvar new = do
		old <- atomically $ swapTVar tvar (Just new)
		let oldbytes = maybe 0 fromBytesProcessed old
		let newbytes = fromBytesProcessed new
		when (newbytes - oldbytes >= mindelta) $ do
			let info' = info { bytesComplete = Just newbytes }
			_ <- tryIO $ updateTransferInfoFile info' tfile'
			atomically $ writeTVar loggedtvar newbytes

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
 - lock and info files, which can be left behind when a transfer
 - process was interrupted.
 -}
checkTransfer :: Transfer -> Annex (Maybe TransferInfo)
checkTransfer t = debugLocks $ do
	(tfile, lck, moldlck) <- fromRepo $ transferFileAndLockFile t
	let deletestale = do
		void $ tryIO $ R.removeLink tfile
		void $ tryIO $ R.removeLink lck
		maybe noop (void . tryIO . R.removeLink) moldlck
#ifndef mingw32_HOST_OS
	v <- getLockStatus lck
	v' <- case (moldlck, v) of
		(Nothing, _) -> pure v
		(_, StatusLockedBy pid) -> pure (StatusLockedBy pid)
		(Just oldlck, _) -> getLockStatus oldlck
	case v' of
		StatusLockedBy pid -> liftIO $ catchDefaultIO Nothing $
			readTransferInfoFile (Just pid) tfile
		_ -> do
			mode <- annexFileMode
			-- Ignore failure due to permissions, races, etc.
			void $ tryIO $ tryLockExclusive (Just mode) lck >>= \case
				Just lockhandle -> case moldlck of
					Nothing -> liftIO $ do
						deletestale
						dropLock lockhandle
					Just oldlck -> tryLockExclusive (Just mode) oldlck >>= \case
						Just oldlockhandle -> liftIO $ do
							deletestale
							dropLock oldlockhandle
							dropLock lockhandle
						Nothing -> liftIO $ dropLock lockhandle
				Nothing -> noop
			return Nothing
#else
	v <- liftIO $ lockShared lck
	liftIO $ case v of
		Nothing -> catchDefaultIO Nothing $
			readTransferInfoFile Nothing tfile
		Just lockhandle -> do
			dropLock lockhandle
			deletestale
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
	findfiles = liftIO . mapM (emptyWhenDoesNotExist . dirContentsRecursive)
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
	findfiles = liftIO . mapM (emptyWhenDoesNotExist . dirContentsRecursive)
		=<< mapM (fromRepo . failedTransferDir u) [Download, Upload]

clearFailedTransfers :: UUID -> Annex [(Transfer, TransferInfo)]
clearFailedTransfers u = do
	failed <- getFailedTransfers u
	mapM_ (removeFailedTransfer . fst) failed
	return failed

removeFailedTransfer :: Transfer -> Annex ()
removeFailedTransfer t = do
	f <- fromRepo $ failedTransferFile t
	liftIO $ void $ tryIO $ R.removeLink f

recordFailedTransfer :: Transfer -> TransferInfo -> Annex ()
recordFailedTransfer t info = do
	failedtfile <- fromRepo $ failedTransferFile t
	writeTransferInfoFile info failedtfile

{- The transfer information file and transfer lock file 
 - to use for a given Transfer. 
 -
 - The transfer lock file used for an Upload includes the UUID.
 - This allows multiple transfers of the same key to different remote
 - repositories run at the same time, while preventing multiple
 - transfers of the same key to the same remote repository.
 -
 - The transfer lock file used for a Download does not include the UUID.
 - This prevents multiple transfers of the same key into the local
 - repository at the same time.
 -
 - Since old versions of git-annex (10.20240227 and older) used to 
 - include the UUID in the transfer lock file for a Download, this also
 - returns a second lock file for Downloads, which has to be locked
 - in order to interoperate with the old git-annex processes.
 - Lock order is the same as return value order. 
 - At some point in the future, when old git-annex processes are no longer
 - a concern, this complication can be removed.
 -}
transferFileAndLockFile :: Transfer -> Git.Repo -> (RawFilePath, RawFilePath, Maybe RawFilePath)
transferFileAndLockFile (Transfer direction u kd) r =
	case direction of
		Upload -> (transferfile, uuidlockfile, Nothing)
		Download -> (transferfile, nouuidlockfile, Just uuidlockfile)
  where
	td = transferDir direction r
	fu = B8.filter (/= '/') (fromUUID u)
	kf = keyFile (mkKey (const kd))
	lckkf = "lck." <> kf
	transferfile = td P.</> fu P.</> kf
	uuidlockfile = td P.</> fu P.</> lckkf
	nouuidlockfile = td P.</> "lck" P.</> lckkf

{- The transfer information file to use to record a failed Transfer -}
failedTransferFile :: Transfer -> Git.Repo -> RawFilePath
failedTransferFile (Transfer direction u kd) r = 
	failedTransferDir u direction r
		P.</> keyFile (mkKey (const kd))

{- Parses a transfer information filename to a Transfer. -}
parseTransferFile :: RawFilePath -> Maybe Transfer
parseTransferFile file
	| "lck." `B.isPrefixOf` P.takeFileName file = Nothing
	| otherwise = case drop (length bits - 3) bits of
		[direction, u, key] -> Transfer
			<$> parseDirection direction
			<*> pure (toUUID u)
			<*> fmap (fromKey id) (fileKey key)
		_ -> Nothing
  where
	bits = P.splitDirectories file

writeTransferInfoFile :: TransferInfo -> RawFilePath -> Annex ()
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

readTransferInfoFile :: Maybe PID -> RawFilePath -> IO (Maybe TransferInfo)
readTransferInfoFile mpid tfile = catchDefaultIO Nothing $
	readTransferInfo mpid . decodeBS <$> F.readFile' (toOsPath tfile)

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
	(firstliner, otherlines) = separate (== '\n') s
	(secondliner, rest) = separate (== '\n') otherlines
	firstline = dropWhileEnd (== '\r') firstliner
	secondline = dropWhileEnd (== '\r') secondliner
	secondline = 
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
		then Just <$> parsePOSIXTime . encodeBS =<< headMaybe bits
		else pure Nothing -- not failure
	bytes = if numbits > 1
		then Just <$> readish =<< headMaybe (drop 1 bits)
		else pure Nothing -- not failure

{- The directory holding transfer information files for a given Direction. -}
transferDir :: Direction -> Git.Repo -> RawFilePath
transferDir direction r = gitAnnexTransferDir r P.</> formatDirection direction

{- The directory holding failed transfer information files for a given
 - Direction and UUID -}
failedTransferDir :: UUID -> Direction -> Git.Repo -> RawFilePath
failedTransferDir u direction r = gitAnnexTransferDir r
	P.</> "failed"
	P.</> formatDirection direction
	P.</> B8.filter (/= '/') (fromUUID u)

prop_read_write_transferinfo :: TransferInfo -> Bool
prop_read_write_transferinfo info
	| isJust (transferRemote info) = True -- remote not stored
	| isJust (transferTid info) = True -- tid not stored
	| otherwise = Just (info { transferPaused = False }) == info'
  where
	info' = readTransferInfo (transferPid info) (writeTransferInfo info)


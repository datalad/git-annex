{- git-annex transfer information files and lock files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Transfer where

import Common.Annex
import Annex.Perms
import Annex.Exception
import qualified Git
import Types.Remote
import Types.Key
import Utility.Percentage

import System.Posix.Types
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import Control.Concurrent

{- Enough information to uniquely identify a transfer, used as the filename
 - of the transfer information file. -}
data Transfer = Transfer
	{ transferDirection :: Direction
	, transferUUID :: UUID
	, transferKey :: Key
	}
	deriving (Eq, Ord, Read, Show)

{- Information about a Transfer, stored in the transfer information file.
 -
 - Note that the associatedFile may not correspond to a file in the local
 - git repository. It's some file, possibly relative to some directory,
 - of some repository, that was acted on to initiate the transfer.
 -}
data TransferInfo = TransferInfo
	{ startedTime :: Maybe POSIXTime
	, transferPid :: Maybe ProcessID
	, transferTid :: Maybe ThreadId
	, transferRemote :: Maybe Remote
	, bytesComplete :: Maybe Integer
	, associatedFile :: Maybe FilePath
	, transferPaused :: Bool
	}
	deriving (Show, Eq, Ord)

data Direction = Upload | Download
	deriving (Eq, Ord, Read, Show)

showLcDirection :: Direction -> String
showLcDirection Upload = "upload"
showLcDirection Download = "download"

readLcDirection :: String -> Maybe Direction
readLcDirection "upload" = Just Upload
readLcDirection "download" = Just Download
readLcDirection _ = Nothing

percentComplete :: Transfer -> TransferInfo -> Maybe Percentage
percentComplete (Transfer { transferKey = key }) (TransferInfo { bytesComplete = Just complete }) =
	(\size -> percentage size complete) <$> keySize key
percentComplete _ _ = Nothing

upload :: UUID -> Key -> AssociatedFile -> Annex Bool -> Annex Bool
upload u key file a = runTransfer (Transfer Upload u key) file a

download :: UUID -> Key -> AssociatedFile -> Annex Bool -> Annex Bool
download u key file a = runTransfer (Transfer Download u key) file a

{- Runs a transfer action. Creates and locks the lock file while the
 - action is running, and stores info in the transfer information
 - file. Will throw an error if the transfer is already in progress.
 -
 - If the transfer action returns False, the transfer info is 
 - left in the failedTransferDir.
 -}
runTransfer :: Transfer -> Maybe FilePath -> Annex Bool -> Annex Bool
runTransfer t file a = do
	tfile <- fromRepo $ transferFile t
	createAnnexDirectory $ takeDirectory tfile
	mode <- annexFileMode
	info <- liftIO $ TransferInfo
		<$> (Just . utcTimeToPOSIXSeconds <$> getCurrentTime)
		<*> pure Nothing -- pid not stored in file, so omitted for speed
		<*> pure Nothing -- tid ditto
		<*> pure Nothing -- not 0; transfer may be resuming
		<*> pure Nothing
		<*> pure file
		<*> pure False
	let content = writeTransferInfo info
	ok <- bracketIO (prep tfile mode content) (cleanup tfile) a
	unless ok $ failed content
	return ok
	where
		prep tfile mode content = do
			fd <- openFd (transferLockFile tfile) ReadWrite (Just mode)
				defaultFileFlags { trunc = True }
			locked <- catchMaybeIO $
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
			when (locked == Nothing) $
				error $ "transfer already in progress"
			writeFile tfile content
			return fd
		cleanup tfile fd = do
			void $ tryIO $ removeFile tfile
			void $ tryIO $ removeFile $ transferLockFile tfile
			closeFd fd
		failed content = do
			failedtfile <- fromRepo $ failedTransferFile t
			createAnnexDirectory $ takeDirectory failedtfile
			liftIO $ writeFile failedtfile content

{- If a transfer is still running, returns its TransferInfo. -}
checkTransfer :: Transfer -> Annex (Maybe TransferInfo)
checkTransfer t = do
	mode <- annexFileMode
	tfile <- fromRepo $ transferFile t
	mfd <- liftIO $ catchMaybeIO $
		openFd (transferLockFile tfile) ReadOnly (Just mode) defaultFileFlags
	case mfd of
		Nothing -> return Nothing -- failed to open file; not running
		Just fd -> do
			locked <- liftIO $
				getLock fd (WriteLock, AbsoluteSeek, 0, 0)
			liftIO $ closeFd fd
			case locked of
				Nothing -> return Nothing
				Just (pid, _) -> liftIO $
					flip catchDefaultIO Nothing $ do
						readTransferInfo (Just pid)
							<$> readFile tfile

{- Gets all currently running transfers. -}
getTransfers :: Annex [(Transfer, TransferInfo)]
getTransfers = do
	transfers <- catMaybes . map parseTransferFile . concat <$> findfiles
	infos <- mapM checkTransfer transfers
	return $ map (\(t, Just i) -> (t, i)) $
		filter running $ zip transfers infos
	where
		findfiles = liftIO . mapM dirContentsRecursive
			=<< mapM (fromRepo . transferDir)
				[Download, Upload]
		running (_, i) = isJust i

{- Gets failed transfers for a given remote UUID. -}
getFailedTransfers :: UUID -> Annex [(Transfer, TransferInfo)]
getFailedTransfers u = catMaybes <$> (liftIO . getpairs =<< concat <$> findfiles)
	where
		getpairs = mapM $ \f -> do
			let mt = parseTransferFile f
			mi <- readTransferInfo Nothing <$> readFile f
			return $ case (mt, mi) of
				(Just t, Just i) -> Just (t, i)
				_ -> Nothing
		findfiles = liftIO . mapM dirContentsRecursive
			=<< mapM (fromRepo . failedTransferDir u)
				[Download, Upload]

{- The transfer information file to use for a given Transfer. -}
transferFile :: Transfer -> Git.Repo -> FilePath
transferFile (Transfer direction u key) r = transferDir direction r
	</> fromUUID u
	</> keyFile key

{- The transfer information file to use to record a failed Transfer -}
failedTransferFile :: Transfer -> Git.Repo -> FilePath
failedTransferFile (Transfer direction u key) r = failedTransferDir u direction r
	</> keyFile key

{- The transfer lock file corresponding to a given transfer info file. -}
transferLockFile :: FilePath -> FilePath
transferLockFile infofile = let (d,f) = splitFileName infofile in
	combine d ("lck." ++ f)

{- Parses a transfer information filename to a Transfer. -}
parseTransferFile :: FilePath -> Maybe Transfer
parseTransferFile file
	| "lck." `isPrefixOf` (takeFileName file) = Nothing
	| otherwise = case drop (length bits - 3) bits of
		[direction, u, key] -> Transfer
			<$> readLcDirection direction
			<*> pure (toUUID u)
			<*> fileKey key
		_ -> Nothing
	where
		bits = splitDirectories file

writeTransferInfo :: TransferInfo -> String
writeTransferInfo info = unlines
	-- transferPid is not included; instead obtained by looking at
	-- the process that locks the file.
	[ maybe "" show $ startedTime info
	-- bytesComplete is not included; changes too fast 
	, fromMaybe "" $ associatedFile info -- comes last; arbitrary content
	]

readTransferInfo :: (Maybe ProcessID) -> String -> Maybe TransferInfo
readTransferInfo mpid s =
	case bits of
		[time] -> TransferInfo
			<$> (Just <$> parsePOSIXTime time)
			<*> pure mpid
			<*> pure Nothing
			<*> pure Nothing
			<*> pure Nothing
			<*> pure (if null filename then Nothing else Just filename)
			<*> pure False
		_ -> Nothing
	where
		(bits, filebits) = splitAt 1 $ lines s 
		filename = join "\n" filebits

parsePOSIXTime :: String -> Maybe POSIXTime
parsePOSIXTime s = utcTimeToPOSIXSeconds
	<$> parseTime defaultTimeLocale "%s%Qs" s

{- The directory holding transfer information files for a given Direction. -}
transferDir :: Direction -> Git.Repo -> FilePath
transferDir direction r = gitAnnexTransferDir r </> showLcDirection direction

{- The directory holding failed transfer information files for a given
 - Direction and UUID -}
failedTransferDir :: UUID -> Direction -> Git.Repo -> FilePath
failedTransferDir u direction r = gitAnnexTransferDir r
	</> "failed"
	</> showLcDirection direction
	</> fromUUID u

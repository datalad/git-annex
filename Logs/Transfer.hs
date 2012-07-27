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
import qualified Fields
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
	deriving (Show, Eq, Ord)

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
	}
	deriving (Show, Eq, Ord)

data Direction = Upload | Download
	deriving (Eq, Ord)

instance Show Direction where
	show Upload = "upload"
	show Download = "download"

readDirection :: String -> Maybe Direction
readDirection "upload" = Just Upload
readDirection "download" = Just Download
readDirection _ = Nothing

percentComplete :: Transfer -> TransferInfo -> Maybe Percentage
percentComplete (Transfer { transferKey = key }) (TransferInfo { bytesComplete = Just complete }) =
	(\size -> percentage size complete) <$> keySize key
percentComplete _ _ = Nothing

upload :: UUID -> Key -> AssociatedFile -> Annex a -> Annex a
upload u key file a = runTransfer (Transfer Upload u key) file a

download :: UUID -> Key -> AssociatedFile -> Annex a -> Annex a
download u key file a = runTransfer (Transfer Download u key) file a

fieldTransfer :: Direction -> Key -> Annex a -> Annex a
fieldTransfer direction key a = do
	afile <- Fields.getField Fields.associatedFile
	maybe a (\u -> runTransfer (Transfer direction (toUUID u) key) afile a)
		=<< Fields.getField Fields.remoteUUID

{- Runs a transfer action. Creates and locks the lock file while the
 - action is running, and stores info in the transfer information
 - file. Will throw an error if the transfer is already in progress.
 -}
runTransfer :: Transfer -> Maybe FilePath -> Annex a -> Annex a
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
	bracketIO (prep tfile mode info) (cleanup tfile) a
	where
		prep tfile mode info = do
			fd <- openFd (transferLockFile tfile) ReadWrite (Just mode)
				defaultFileFlags { trunc = True }
			locked <- catchMaybeIO $
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
			when (locked == Nothing) $
				error $ "transfer already in progress"
			writeFile tfile $ writeTransferInfo info
			return fd
		cleanup tfile fd = do
			removeFile tfile
			removeFile $ transferLockFile tfile
			closeFd fd

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
						readTransferInfo pid 
							<$> readFile tfile

{- Gets all currently running transfers. -}
getTransfers :: Annex [(Transfer, TransferInfo)]
getTransfers = do
	transfers <- catMaybes . map parseTransferFile <$> findfiles
	infos <- mapM checkTransfer transfers
	return $ map (\(t, Just i) -> (t, i)) $
		filter running $ zip transfers infos
	where
		findfiles = liftIO . dirContentsRecursive
			=<< fromRepo gitAnnexTransferDir
		running (_, i) = isJust i

{- The transfer information file to use for a given Transfer. -}
transferFile :: Transfer -> Git.Repo -> FilePath
transferFile (Transfer direction u key) r = gitAnnexTransferDir r
	</> show direction 
	</> fromUUID u
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
			<$> readDirection direction
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

readTransferInfo :: ProcessID -> String -> Maybe TransferInfo
readTransferInfo pid s =
	case bits of
		[time] -> TransferInfo
			<$> (Just <$> parsePOSIXTime time)
			<*> pure (Just pid)
			<*> pure Nothing
			<*> pure Nothing
			<*> pure Nothing
			<*> pure (if null filename then Nothing else Just filename)
		_ -> Nothing
	where
		(bits, filebits) = splitAt 1 $ lines s 
		filename = join "\n" filebits

parsePOSIXTime :: String -> Maybe POSIXTime
parsePOSIXTime s = utcTimeToPOSIXSeconds
	<$> parseTime defaultTimeLocale "%s%Qs" s

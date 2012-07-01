{- git-annex transfer log files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Transfer where

import Common.Annex
import Types.Remote
import Remote
import Annex.Perms
import Annex.Exception
import qualified Git

import qualified Data.Map as M
import Control.Concurrent
import System.Posix.Process
import System.Posix.Types
import Data.Time.Clock

{- Enough information to uniquely identify a transfer, used as the filename
 - of the transfer information file. -}
data Transfer = Transfer Direction Remote Key
	deriving (Show)

{- Information about a Transfer, stored in the transfer information file. -}
data TransferInfo = TransferInfo
	{ transferPid :: Maybe ProcessID
	, transferThread :: Maybe ThreadId
	, startedTime :: UTCTime
	, bytesComplete :: Maybe Integer
	, associatedFile :: Maybe FilePath
	}
	deriving (Show)

data Direction = Upload | Download

instance Show Direction where
	show Upload = "upload"
	show Download = "download"

readDirection :: String -> Maybe Direction
readDirection "upload" = Just Upload
readDirection "download" = Just Download
readDirection _ = Nothing

{- Runs a transfer action. Creates and locks the transfer information file
 - while the action is running. Will throw an error if the transfer is
 - already in progress.
 -}
transfer :: Transfer -> Maybe FilePath -> Annex a -> Annex a
transfer transfer file a = do
	createAnnexDirectory =<< fromRepo gitAnnexTransferDir
	tfile <- fromRepo $ transferFile transfer
	mode <- annexFileMode
	info <- liftIO $ TransferInfo
		<$> pure Nothing -- pid not stored in file, so omitted for speed
		<*> pure Nothing -- threadid not stored in file, so omitted for speed
		<*> getCurrentTime
		<*> pure Nothing -- not 0; transfer may be resuming
		<*> pure file
	bracketIO (setup tfile mode info) (cleanup tfile) a
	where
		setup tfile mode info = do
			fd <- openFd tfile ReadWrite (Just mode)
				defaultFileFlags { trunc = True }
			locked <- catchMaybeIO $
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
			when (locked == Nothing) $
				error $ "transfer already in progress"
			fdWrite fd $ writeTransferInfo info
			return fd
		cleanup tfile fd = do
			removeFile tfile
			closeFd fd

{- If a transfer is still running, returns its TransferInfo. -}
checkTransfer :: Transfer -> Annex (Maybe TransferInfo)
checkTransfer transfer = do
	mode <- annexFileMode
	tfile <- fromRepo $ transferFile transfer
	mfd <- liftIO $ catchMaybeIO $
		openFd tfile ReadOnly (Just mode) defaultFileFlags
	case mfd of
		Nothing -> return Nothing -- failed to open file; not running
		Just fd -> do
			locked <- liftIO $
				getLock fd (WriteLock, AbsoluteSeek, 0, 0)
			case locked of
				Nothing -> do
					liftIO $ closeFd fd
					return Nothing
				Just (pid, _) -> liftIO $ do
					handle <- fdToHandle fd
					info <- readTransferInfo pid
						<$> hGetContentsStrict handle
					closeFd fd
					return info

{- Gets all currently running transfers. -}
getTransfers :: Annex [(Transfer, TransferInfo)]
getTransfers = do
	uuidmap <- remoteMap id
	transfers <- catMaybes . map (parseTransferFile uuidmap) <$> findfiles
	infos <- mapM checkTransfer transfers
	return $ map (\(t, Just i) -> (t, i)) $
		filter running $ zip transfers infos
	where
		findfiles = liftIO . dirContentsRecursive
			=<< fromRepo gitAnnexTransferDir
		running (_, i) = isJust i

{- The transfer information file to use for a given Transfer. -}
transferFile :: Transfer -> Git.Repo -> FilePath
transferFile (Transfer direction remote key) repo = 
	gitAnnexTransferDir repo 
		</> show direction 
		</> show (uuid remote) 
		</> keyFile key

{- Parses a transfer information filename to a Transfer. -}
parseTransferFile :: M.Map UUID Remote -> FilePath -> Maybe Transfer
parseTransferFile uuidmap file = 
	case drop (length bits - 3) bits of
		[direction, uuid, key] -> Transfer
			<$> readDirection direction
			<*> M.lookup (toUUID uuid) uuidmap
			<*> fileKey key
		_ -> Nothing
	where
		bits = splitDirectories file

writeTransferInfo :: TransferInfo -> String
writeTransferInfo info = unwords
	-- transferPid is not included; instead obtained by looking at
	-- the process that locks the file.
	-- transferThread is not included; not relevant for other processes
	[ show $ startedTime info
	-- bytesComplete is not included; changes too fast 
	, fromMaybe "" $ associatedFile info -- comes last, may contain spaces
	]

readTransferInfo :: ProcessID -> String -> Maybe TransferInfo
readTransferInfo pid s =
	case bits of
		[time] -> TransferInfo
			<$> pure (Just pid)
			<*> pure Nothing
			<*> readish time
			<*> pure Nothing
			<*> pure filename
		_ -> Nothing
	where
		(bits, filebits) = splitAt 1 $ split " " s 
		filename
			| null filebits = Nothing
			| otherwise = Just $ join " " filebits

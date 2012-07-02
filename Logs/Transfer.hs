{- git-annex transfer information files
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
import qualified Fields

import Control.Concurrent
import System.Posix.Types
import Data.Time.Clock

{- Enough information to uniquely identify a transfer, used as the filename
 - of the transfer information file. -}
data Transfer = Transfer
	{ transferDirection :: Direction
	, transferRemote :: UUID
	, transferKey :: Key
	}
	deriving (Show, Eq, Ord)

{- Information about a Transfer, stored in the transfer information file. -}
data TransferInfo = TransferInfo
	{ startedTime :: UTCTime
	, transferPid :: Maybe ProcessID
	, transferThread :: Maybe ThreadId
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

upload :: UUID -> Key -> AssociatedFile -> Annex a -> Annex a
upload u key file a = transfer (Transfer Upload u key) file a

download :: UUID -> Key -> AssociatedFile -> Annex a -> Annex a
download u key file a = transfer (Transfer Download u key) file a

fieldTransfer :: Direction -> Key -> Annex a -> Annex a
fieldTransfer direction key a = do
	afile <- Fields.getField Fields.associatedFile
	maybe a (\u -> transfer (Transfer direction (toUUID u) key) afile a)
		=<< Fields.getField Fields.remoteUUID

{- Runs a transfer action. Creates and locks the transfer information file
 - while the action is running. Will throw an error if the transfer is
 - already in progress.
 -}
transfer :: Transfer -> Maybe FilePath -> Annex a -> Annex a
transfer t file a = do
	tfile <- fromRepo $ transferFile t
	createAnnexDirectory $ takeDirectory tfile
	mode <- annexFileMode
	info <- liftIO $ TransferInfo
		<$> getCurrentTime
		<*> pure Nothing -- pid not stored in file, so omitted for speed
		<*> pure Nothing -- threadid not stored in file, so omitted for speed
		<*> pure Nothing -- not 0; transfer may be resuming
		<*> pure file
	bracketIO (prep tfile mode info) (cleanup tfile) a
	where
		prep tfile mode info = do
			fd <- openFd tfile ReadWrite (Just mode)
				defaultFileFlags { trunc = True }
			locked <- catchMaybeIO $
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
			when (locked == Nothing) $
				error $ "transfer already in progress"
			h <- fdToHandle fd
			hPutStr h $ writeTransferInfo info
			hFlush h
			return h
		cleanup tfile h = do
			removeFile tfile
			hClose h

{- If a transfer is still running, returns its TransferInfo. -}
checkTransfer :: Transfer -> Annex (Maybe TransferInfo)
checkTransfer t = do
	mode <- annexFileMode
	tfile <- fromRepo $ transferFile t
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
					h <- fdToHandle fd
					info <- readTransferInfo pid
						<$> hGetContentsStrict h
					hClose h
					return info

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

{- Parses a transfer information filename to a Transfer. -}
parseTransferFile :: FilePath -> Maybe Transfer
parseTransferFile file = 
	case drop (length bits - 3) bits of
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
	-- transferThread is not included; not relevant for other processes
	[ show $ startedTime info
	-- bytesComplete is not included; changes too fast 
	, fromMaybe "" $ associatedFile info -- comes last; arbitrary content
	]

readTransferInfo :: ProcessID -> String -> Maybe TransferInfo
readTransferInfo pid s =
	case bits of
		[time] -> TransferInfo
			<$> readish time
			<*> pure (Just pid)
			<*> pure Nothing
			<*> pure Nothing
			<*> pure filename
		_ -> Nothing
	where
		(bits, filebits) = splitAt 1 $ lines s 
		filename
			| null filebits = Nothing
			| otherwise = Just $ join "\n" filebits

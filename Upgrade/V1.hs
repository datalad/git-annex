{- git-annex v1 -> v2 upgrade support
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V1 where

import System.Posix.Types
import Data.Char
import Data.Default
import Data.ByteString.Builder
import qualified Data.ByteString as S
import qualified Data.ByteString.Short as S (toShort, fromShort)
import System.PosixCompat.Files (isRegularFile)
import Text.Read

import Annex.Common
import Types.Upgrade
import Annex.Content
import Annex.Link
import Annex.Perms
import Types.Key
import Logs.Presence
import qualified Annex.Queue
import qualified Git
import qualified Git.LsFiles as LsFiles
import Backend
import Utility.FileMode
import Utility.Tmp
import qualified Upgrade.V2
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F

-- v2 adds hashing of filenames of content and location log files.
-- Key information is encoded in filenames differently, so
-- both content and location log files move around, and symlinks
-- to content need to be changed.
-- 
-- When upgrading a v1 key to v2, file size metadata ought to be
-- added to the key (unless it is a WORM key, which encoded
-- mtime:size in v1). This can only be done when the file content
-- is present. Since upgrades need to happen consistently, 
-- (so that two repos get changed the same way by the upgrade, and
-- will merge), that metadata cannot be added on upgrade.
--
-- Note that file size metadata
-- will only be used for detecting situations where git-annex
-- would run out of disk space, so if some keys don't have it,
-- the impact is minor. At least initially. It could be used in the
-- future by smart auto-repo balancing code, etc.
--
-- Anyway, since v2 plans ahead for other metadata being included
-- in keys, there should probably be a way to update a key.
-- Something similar to the migrate subcommand could be used,
-- and users could then run that at their leisure.

upgrade :: Annex UpgradeResult
upgrade = do
	showAction "v1 to v2"
	
	ifM (fromRepo Git.repoIsLocalBare)
		( moveContent
		, do
			moveContent
			updateSymlinks
			moveLocationLogs
	
			Annex.Queue.flush
		)
	
	Upgrade.V2.upgrade

moveContent :: Annex ()
moveContent = do
	showAction "moving content"
	files <- getKeyFilesPresent1
	forM_ files move
  where
	move f = do
		let k = fileKey1 (fromOsPath $ takeFileName f)
		let d = parentDir f
		liftIO $ allowWrite d
		liftIO $ allowWrite f
		_ <- moveAnnex k (AssociatedFile Nothing) f
		liftIO $ removeDirectory d

updateSymlinks :: Annex ()
updateSymlinks = do
	showAction "updating symlinks"
	top <- fromRepo Git.repoPath
	(files, cleanup) <- inRepo $ LsFiles.inRepo [] [top]
	forM_ files fixlink
	void $ liftIO cleanup
  where
	fixlink f = do
		r <- lookupKey1 f
		case r of
			Nothing -> noop
			Just (k, _) -> do
				link <- calcRepo (gitAnnexLink f k)
				liftIO $ removeFile f
				liftIO $ R.createSymbolicLink (fromOsPath link) (fromOsPath f)
				Annex.Queue.addCommand [] "add" [Param "--"] [(fromOsPath f)]

moveLocationLogs :: Annex ()
moveLocationLogs = do
	showAction "moving location logs"
	logkeys <- oldlocationlogs
	forM_ logkeys move
  where
	oldlocationlogs = do
		dir <- fromRepo Upgrade.V2.gitStateDir
		ifM (liftIO $ doesDirectoryExist dir)
			( mapMaybe (oldlog2key . fromOsPath)
				<$> liftIO (getDirectoryContents dir)
			, return []
			)
	move (l, k) = do
		dest <- fromRepo (logFile2 k)
		dir <- fromRepo Upgrade.V2.gitStateDir
		let f = dir </> toOsPath l
		createWorkTreeDirectory (parentDir dest)
		-- could just git mv, but this way deals with
		-- log files that are not checked into git,
		-- as well as merging with already upgraded
		-- logs that have been pulled from elsewhere
		old <- liftIO $ readLog1 f
		new <- liftIO $ readLog1 dest
		liftIO $ writeLog1 dest (old++new)
		Annex.Queue.addCommand [] "add" [Param "--"] [fromOsPath dest]
		Annex.Queue.addCommand [] "add" [Param "--"] [fromOsPath f]
		Annex.Queue.addCommand [] "rm" [Param "--quiet", Param "-f", Param "--"] [fromOsPath f]

oldlog2key :: FilePath -> Maybe (FilePath, Key)
oldlog2key l
	| drop len l == ".log" && sane = Just (l, k)
	| otherwise = Nothing
  where
	len = length l - 4
	k = readKey1 (take len l)
	sane = (not . S.null $ S.fromShort $ fromKey keyName k) && (not . S.null $ formatKeyVariety $ fromKey keyVariety k)

-- WORM backend keys: "WORM:mtime:size:filename"
-- all the rest: "backend:key"
--
-- If the file looks like "WORM:FOO-...", then it was created by mixing
-- v2 and v1; that infelicity is worked around by treating the value
-- as the v2 key that it is.
readKey1 :: String -> Key
readKey1 = fromMaybe (giveup "unable to parse v0 key") . readKey1'

readKey1' :: String -> Maybe Key
readKey1' v
	| mixup = deserializeKey $ intercalate ":" $ drop 1 bits
	| otherwise = Just $ mkKey $ \d -> d
		{ keyName = S.toShort (encodeBS n)
		, keyVariety = parseKeyVariety (encodeBS b)
		, keySize = s
		, keyMtime = t
		}
  where
	bits = splitc ':' v
	b = fromMaybe (error "unable to parse v0 key") (headMaybe bits)
	n = intercalate ":" $ drop (if wormy then 3 else 1) bits
	t = if wormy
		then readMaybe (bits !! 1) :: Maybe EpochTime
		else Nothing
	s = if wormy && length bits > 2
		then readMaybe (bits !! 2) :: Maybe Integer
		else Nothing
	wormy = length bits > 1 && headMaybe bits == Just "WORM"
	mixup = wormy && fromMaybe False (isUpper <$> (headMaybe $ bits !! 1))

showKey1 :: Key -> String
showKey1 k = intercalate ":" $ filter (not . null)
	[b, showifhere t, showifhere s, decodeBS n]
  where
	showifhere Nothing = ""
	showifhere (Just x) = show x
	b = decodeBS $ formatKeyVariety v
	n = S.fromShort $ fromKey keyName k
	v = fromKey keyVariety k
	s = fromKey keySize k
	t = fromKey keyMtime k

keyFile1 :: Key -> FilePath
keyFile1 key = replace "/" "%" $ replace "%" "&s" $ replace "&" "&a"  $ showKey1 key

fileKey1 :: FilePath -> Key
fileKey1 file = readKey1 $
	replace "&a" "&" $ replace "&s" "%" $ replace "%" "/" file

writeLog1 :: OsPath -> [LogLine] -> IO ()
writeLog1 file ls = viaTmp F.writeFile file (toLazyByteString $ buildLog ls)

readLog1 :: OsPath -> IO [LogLine]
readLog1 file = catchDefaultIO [] $ parseLog <$> F.readFile file

lookupKey1 :: OsPath -> Annex (Maybe (Key, Backend))
lookupKey1 file = do
	tl <- liftIO $ tryIO getsymlink
	case tl of
		Left _ -> return Nothing
		Right l -> makekey l
  where
	getsymlink :: IO OsPath
	getsymlink = takeFileName . toOsPath
		<$> R.readSymbolicLink (fromOsPath file)
	makekey l = maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
		Nothing -> do
			unless (null kname || null bname ||
			        not (isLinkToAnnex (fromOsPath l))) $
				warning (UnquotedString skip)
			return Nothing
		Just backend -> return $ Just (k, backend)
	  where
		k = fileKey1 (fromOsPath l)
		bname = decodeBS (formatKeyVariety (fromKey keyVariety k))
		kname = decodeBS (S.fromShort (fromKey keyName k))
		skip = "skipping " ++ fromOsPath file ++ 
			" (unknown backend " ++ bname ++ ")"

getKeyFilesPresent1 :: Annex [OsPath]
getKeyFilesPresent1  = getKeyFilesPresent1' =<< fromRepo gitAnnexObjectDir
getKeyFilesPresent1' :: OsPath -> Annex [OsPath]
getKeyFilesPresent1' dir =
	ifM (liftIO $ doesDirectoryExist dir)
		(  do
			dirs <- liftIO $ getDirectoryContents dir
			let files = map (\d -> dir <> literalOsPath "/" <> d <> literalOsPath "/" <> takeFileName d) dirs
			liftIO $ filterM present files
		, return []
		)
  where
	present :: OsPath -> IO Bool
	present f = do
		result <- tryIO $ R.getFileStatus (fromOsPath f)
		case result of
			Right s -> return $ isRegularFile s
			Left _ -> return False

logFile2 :: Key -> Git.Repo -> OsPath
logFile2 = logFile' (hashDirLower def)

logFile' :: (Key -> OsPath) -> Key -> Git.Repo -> OsPath
logFile' hasher key repo =
	gitStateDir repo <> hasher key <> keyFile key <> literalOsPath ".log"

stateDir :: OsPath
stateDir = addTrailingPathSeparator (literalOsPath ".git-annex")

gitStateDir :: Git.Repo -> OsPath
gitStateDir repo = addTrailingPathSeparator $ Git.repoPath repo </> stateDir

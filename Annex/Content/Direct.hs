{- git-annex file content managing for direct mode
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Content.Direct (
	associatedFiles,
	removeAssociatedFile,
	addAssociatedFile,
	goodContent,
	changedFileStatus,
	updateCache,
	recordedCache,
	compareCache,
	writeCache,
	removeCache,
	genCache,
	toCache,
	Cache(..),
	prop_read_show_direct
) where

import Common.Annex
import qualified Git
import Utility.TempFile
import Logs.Location

import System.Posix.Types

{- Files in the tree that are associated with a key. -}
associatedFiles :: Key -> Annex [FilePath]
associatedFiles key = do
	files <- associatedFilesRelative key
	top <- fromRepo Git.repoPath
	return $ map (top </>) files

{- List of files in the tree that are associated with a key, relative to
 - the top of the repo. -}
associatedFilesRelative :: Key -> Annex [FilePath] 
associatedFilesRelative key = do
	mapping <- inRepo $ gitAnnexMapping key
	liftIO $ catchDefaultIO [] $ lines <$> readFile mapping

{- Changes the associated files information for a key, applying a
 - transformation to the list. Returns a copy of the new info. -}
changeAssociatedFiles :: Key -> ([FilePath] -> [FilePath]) -> Annex [FilePath]
changeAssociatedFiles key transform = do
	mapping <- inRepo $ gitAnnexMapping key
	files <- associatedFilesRelative key
	let files' = transform files
	when (files /= files') $
		liftIO $ viaTmp writeFile mapping $ unlines files'
	return files'

removeAssociatedFile :: Key -> FilePath -> Annex [FilePath]
removeAssociatedFile key file = do
	fs <- changeAssociatedFiles key $ filter (/= normalise file)
	when (null fs) $
		logStatus key InfoMissing
	return fs

addAssociatedFile :: Key -> FilePath -> Annex [FilePath]
addAssociatedFile key file = changeAssociatedFiles key $ \files ->
	if file' `elem` files
		then files
		else file':files
  where
	file' = normalise file

{- Checks if a file in the tree, associated with a key, has not been modified.
 -
 - To avoid needing to fsck the file's content, which can involve an
 - expensive checksum, this relies on a cache that contains the file's
 - expected mtime and inode.
 -}
goodContent :: Key -> FilePath -> Annex Bool
goodContent key file = do
	old <- recordedCache key
	compareCache file old

changedFileStatus :: Key -> FileStatus -> Annex Bool
changedFileStatus key status = do
	old <- recordedCache key
	let curr = toCache status
	return $ curr /= old

{- Gets the recorded cache for a key. -}
recordedCache :: Key -> Annex (Maybe Cache)
recordedCache key = withCacheFile key $ \cachefile ->
	catchDefaultIO Nothing $ readCache <$> readFile cachefile

{- Compares a cache with the current cache for a file. -}
compareCache :: FilePath -> Maybe Cache -> Annex Bool
compareCache file old = do
	curr <- liftIO $ genCache file
	return $ isJust curr && curr == old

{- Stores a cache of attributes for a file that is associated with a key. -}
updateCache :: Key -> FilePath -> Annex ()
updateCache key file = maybe noop (writeCache key) =<< liftIO (genCache file)

{- Writes a cache for a key. -}
writeCache :: Key -> Cache -> Annex ()
writeCache key cache = withCacheFile key $ \cachefile -> do
	createDirectoryIfMissing True (parentDir cachefile)
	writeFile cachefile $ showCache cache

{- Removes a cache. -}
removeCache :: Key -> Annex ()
removeCache key = withCacheFile key nukeFile

{- Cache a file's inode, size, and modification time to determine if it's
 - been changed. -}
data Cache = Cache FileID FileOffset EpochTime
	deriving (Eq, Show)

showCache :: Cache -> String
showCache (Cache inode size mtime) = unwords
	[ show inode
	, show size
	, show mtime
	]

readCache :: String -> Maybe Cache
readCache s = case words s of
	(inode:size:mtime:_) -> Cache
		<$> readish inode
		<*> readish size
		<*> readish mtime
	_ -> Nothing

-- for quickcheck
prop_read_show_direct :: Cache -> Bool
prop_read_show_direct c = readCache (showCache c) == Just c

genCache :: FilePath -> IO (Maybe Cache)
genCache f = catchDefaultIO Nothing $ toCache <$> getFileStatus f

toCache :: FileStatus -> Maybe Cache
toCache s
	| isRegularFile s = Just $ Cache
		(fileID s)
		(fileSize s)
		(modificationTime s)
	| otherwise = Nothing

withCacheFile :: Key -> (FilePath -> IO a) -> Annex a
withCacheFile key a = liftIO . a =<< inRepo (gitAnnexCache key)

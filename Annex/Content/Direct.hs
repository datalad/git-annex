{- git-annex file content managing for direct mode
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Content.Direct (
	associatedFiles,
	changeAssociatedFiles,
	updateAssociatedFiles,
	goodContent,
	updateCache,
	recordedCache,
	compareCache,
	removeCache
) where

import Common.Annex
import qualified Git
import qualified Git.DiffTree as DiffTree
import Git.Sha
import Annex.CatFile
import Utility.TempFile
import Utility.FileMode

import System.Posix.Types
import qualified Data.ByteString.Lazy as L

{- Files in the tree that are associated with a key.
 -
 - When no known associated files exist, returns the gitAnnexLocation. -}
associatedFiles :: Key -> Annex [FilePath]
associatedFiles key = do
	files <- associatedFilesList key
	if null files
		then do
			l <- inRepo $ gitAnnexLocation key
			return [l]
		else do
			top <- fromRepo Git.repoPath
			return $ map (top </>) files

{- Raw list of files in the tree that are associated with a key. -}
associatedFilesList :: Key -> Annex [FilePath] 
associatedFilesList key = do
	mapping <- inRepo $ gitAnnexMapping key
	liftIO $ catchDefaultIO [] $ lines <$> readFile mapping

{- Changes the associated files information for a key, applying a
 - transformation to the list. -}
changeAssociatedFiles :: Key -> ([FilePath] -> [FilePath]) -> Annex ()
changeAssociatedFiles key transform = do
	mapping <- inRepo $ gitAnnexMapping key
	files <- associatedFilesList key
	let files' = transform files
	when (files /= files') $
		liftIO $ viaTmp writeFile mapping $ unlines files'

removeAssociatedFile :: Key -> FilePath -> Annex ()
removeAssociatedFile key file = changeAssociatedFiles key $ filter (/= file)

addAssociatedFile :: Key -> FilePath -> Annex ()
addAssociatedFile key file = changeAssociatedFiles key $ \files ->
	if file `elem` files
		then files
		else file:files

{- Uses git diff-tree to find files changed between two tree Shas, and
 - updates the associated file mappings, efficiently. -}
updateAssociatedFiles :: Git.Sha -> Git.Sha -> Annex ()
updateAssociatedFiles oldsha newsha = do
	(items, cleanup) <- inRepo $ DiffTree.diffTree oldsha newsha
	forM_ items update
	void $ liftIO $ cleanup
  where
	update item = do
		go DiffTree.dstsha DiffTree.dstmode addAssociatedFile
		go DiffTree.srcsha DiffTree.srcmode removeAssociatedFile
	  where
		go getsha getmode a =
			when (getsha item /= nullSha && isSymLink (getmode item)) $ do
				key <- getkey $ getsha item
				maybe noop (\k -> a k $ DiffTree.file item) key
		getkey sha = fileKey . takeFileName . encodeW8 . L.unpack
			<$> catObject sha

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
updateCache key file = do
	withCacheFile key $ \cachefile -> do
		createDirectoryIfMissing True (parentDir cachefile)
		maybe noop (writeFile cachefile . showCache) =<< genCache file

{- Removes a cache. -}
removeCache :: Key -> Annex ()
removeCache key = withCacheFile key nukeFile

{- Cache a file's inode, size, and modification time to determine if it's
 - been changed. -}
data Cache = Cache FileID FileOffset EpochTime
  deriving (Eq)

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

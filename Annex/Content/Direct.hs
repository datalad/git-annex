{- git-annex file content managing for old direct mode repositories
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Content.Direct (
	goodContent,
	associatedFiles,
	removeAssociatedFiles,
	removeInodeCache,
) where

import Annex.Common
import Annex.Perms
import qualified Git
import Logs.Location
import Logs.File
import Utility.InodeCache
import Utility.CopyFile
import Annex.ReplaceFile
import Annex.Link
import Annex.InodeSentinal

{- Absolute FilePaths of Files in the tree that are associated with a key. -}
associatedFiles :: Key -> Annex [FilePath]
associatedFiles key = do
	files <- associatedFilesRelative key
	top <- fromRepo Git.repoPath
	return $ map (top </>) files

{- List of files in the tree that are associated with a key, relative to
 - the top of the repo. -}
associatedFilesRelative :: Key -> Annex [FilePath] 
associatedFilesRelative key = do
	mapping <- calcRepo $ gitAnnexMapping key
	liftIO $ catchDefaultIO [] $ withFile mapping ReadMode $ \h ->
		-- Read strictly to ensure the file is closed
		-- before changeAssociatedFiles tries to write to it.
		-- (Especially needed on Windows.)
		lines <$> hGetContentsStrict h

{- Removes the list of associated files. -}
removeAssociatedFiles :: Key -> Annex ()
removeAssociatedFiles key = do
	mapping <- calcRepo $ gitAnnexMapping key
	modifyContent mapping $
		liftIO $ nukeFile mapping

{- Checks if a file in the tree, associated with a key, has not been modified.
 -
 - To avoid needing to fsck the file's content, which can involve an
 - expensive checksum, this relies on a cache that contains the file's
 - expected mtime and inode.
 -}
goodContent :: Key -> FilePath -> Annex Bool
goodContent key file = sameInodeCache file =<< recordedInodeCache key

{- Gets the recorded inode cache for a key. 
 -
 - A key can be associated with multiple files, so may return more than
 - one. -}
recordedInodeCache :: Key -> Annex [InodeCache]
recordedInodeCache key = withInodeCacheFile key $ \f ->
	liftIO $ catchDefaultIO [] $
		mapMaybe readInodeCache . lines <$> readFileStrict f

{- Removes an inode cache. -}
removeInodeCache :: Key -> Annex ()
removeInodeCache key = withInodeCacheFile key $ \f ->
	modifyContent f $
		liftIO $ nukeFile f

withInodeCacheFile :: Key -> (FilePath -> Annex a) -> Annex a
withInodeCacheFile key a = a =<< calcRepo (gitAnnexInodeCache key)

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
	recordedInodeCache,
	updateInodeCache,
	writeInodeCache,
	compareInodeCache,
	toInodeCache,
) where

import Common.Annex
import Annex.Perms
import qualified Git
import Utility.TempFile
import Logs.Location
import Utility.InodeCache

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
	mapping <- inRepo $ gitAnnexMapping key
	liftIO $ catchDefaultIO [] $ do
		h <- openFile mapping ReadMode
		fileEncoding h
		lines <$> hGetContents h

{- Changes the associated files information for a key, applying a
 - transformation to the list. Returns new associatedFiles value. -}
changeAssociatedFiles :: Key -> ([FilePath] -> [FilePath]) -> Annex [FilePath]
changeAssociatedFiles key transform = do
	mapping <- inRepo $ gitAnnexMapping key
	files <- associatedFilesRelative key
	let files' = transform files
	when (files /= files') $ do
		createContentDir mapping
		liftIO $ viaTmp write mapping $ unlines files'
	top <- fromRepo Git.repoPath
	return $ map (top </>) files'
  where
	write file content = do
		h <- openFile file WriteMode
		fileEncoding h
 		hPutStr h content
		hClose h

{- Removes an associated file. Returns new associatedFiles value. -}
removeAssociatedFile :: Key -> FilePath -> Annex [FilePath]
removeAssociatedFile key file = do
	file' <- normaliseAssociatedFile file
	fs <- changeAssociatedFiles key $ filter (/= file')
	when (null fs) $
		logStatus key InfoMissing
	return fs

{- Adds an associated file. Returns new associatedFiles value. -}
addAssociatedFile :: Key -> FilePath -> Annex [FilePath]
addAssociatedFile key file = do
	file' <- normaliseAssociatedFile file
	changeAssociatedFiles key $ \files -> do
		if file' `elem` files
			then files
			else file':files

{- Associated files are always stored relative to the top of the repository.
 - The input FilePath is relative to the CWD. -}
normaliseAssociatedFile :: FilePath -> Annex FilePath
normaliseAssociatedFile file = do
	top <- fromRepo Git.repoPath
	liftIO $ relPathDirToFile top <$> absPath file

{- Checks if a file in the tree, associated with a key, has not been modified.
 -
 - To avoid needing to fsck the file's content, which can involve an
 - expensive checksum, this relies on a cache that contains the file's
 - expected mtime and inode.
 -}
goodContent :: Key -> FilePath -> Annex Bool
goodContent key file = do
	old <- recordedInodeCache key
	liftIO $ compareInodeCache file old

changedFileStatus :: Key -> FileStatus -> Annex Bool
changedFileStatus key status = do
	old <- recordedInodeCache key
	let curr = toInodeCache status
	return $ curr /= old

{- Gets the recorded inode cache for a key. -}
recordedInodeCache :: Key -> Annex (Maybe InodeCache)
recordedInodeCache key = withInodeCacheFile key $ \f ->
	liftIO $ catchDefaultIO Nothing $ readInodeCache <$> readFile f

{- Stores a cache of attributes for a file that is associated with a key. -}
updateInodeCache :: Key -> FilePath -> Annex ()
updateInodeCache key file = maybe noop (writeInodeCache key)
	=<< liftIO (genInodeCache file)

{- Writes a cache for a key. -}
writeInodeCache :: Key -> InodeCache -> Annex ()
writeInodeCache key cache = withInodeCacheFile key $ \f -> do
	createContentDir f
	liftIO $ writeFile f $ showInodeCache cache

withInodeCacheFile :: Key -> (FilePath -> Annex a) -> Annex a
withInodeCacheFile key a = a =<< inRepo (gitAnnexInodeCache key)

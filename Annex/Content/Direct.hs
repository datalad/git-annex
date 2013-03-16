{- git-annex file content managing for direct mode
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Content.Direct (
	associatedFiles,
	removeAssociatedFile,
	addAssociatedFile,
	goodContent,
	recordedInodeCache,
	updateInodeCache,
	writeInodeCache,
	compareInodeCaches,
	compareInodeCachesWith,
	sameInodeCache,
	sameFileStatus,
	removeInodeCache,
	toInodeCache,
	inodesChanged,
	createInodeSentinalFile,
) where

import Common.Annex
import qualified Annex
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
	changeAssociatedFiles key $ \files ->
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
goodContent key file = sameInodeCache file =<< recordedInodeCache key

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

{- Removes an inode cache. -}
removeInodeCache :: Key -> Annex ()
removeInodeCache key = withInodeCacheFile key $ \f -> do
	createContentDir f -- also thaws directory
	liftIO $ nukeFile f

withInodeCacheFile :: Key -> (FilePath -> Annex a) -> Annex a
withInodeCacheFile key a = a =<< inRepo (gitAnnexInodeCache key)

{- Checks if a InodeCache matches the current version of a file. -}
sameInodeCache :: FilePath -> Maybe InodeCache -> Annex Bool
sameInodeCache _ Nothing = return False
sameInodeCache file (Just old) = go =<< liftIO (genInodeCache file)
  where
	go Nothing = return False
	go (Just curr) = compareInodeCaches curr old

{- Checks if a FileStatus matches the recorded InodeCache of a file. -}
sameFileStatus :: Key -> FileStatus -> Annex Bool
sameFileStatus key status = do
	old <- recordedInodeCache key
	let curr = toInodeCache status
	r <- case (old, curr) of
		(Just o, Just c) -> compareInodeCaches o c
		(Nothing, Nothing) -> return True
		_ -> return False
	return r

{- If the inodes have changed, only the size and mtime are compared. -}
compareInodeCaches :: InodeCache -> InodeCache -> Annex Bool
compareInodeCaches x y
	| compareStrong x y = return True
	| otherwise = ifM inodesChanged
		( return $ compareWeak x y
		, return False
		)

compareInodeCachesWith :: Annex InodeComparisonType
compareInodeCachesWith = ifM inodesChanged ( return Weakly, return Strongly )

{- Some filesystems get new inodes each time they are mounted.
 - In order to work on such a filesystem, a sentinal file is used to detect
 - when the inodes have changed.
 -
 - If the sentinal file does not exist, we have to assume that the
 - inodes have changed.
 -}
inodesChanged :: Annex Bool
inodesChanged = maybe calc return =<< Annex.getState Annex.inodeschanged
  where
	calc = do
		scache <- liftIO . genInodeCache
			=<< fromRepo gitAnnexInodeSentinal
		scached <- readInodeSentinalFile
		let changed = case (scache, scached) of
			(Just c1, Just c2) -> not $ compareStrong c1 c2
			_ -> True
		Annex.changeState $ \s -> s { Annex.inodeschanged = Just changed }
		return changed

readInodeSentinalFile :: Annex (Maybe InodeCache)
readInodeSentinalFile = do
	sentinalcachefile <- fromRepo gitAnnexInodeSentinalCache
	liftIO $ catchDefaultIO Nothing $
		readInodeCache <$> readFile sentinalcachefile

writeInodeSentinalFile :: Annex ()
writeInodeSentinalFile = do
	sentinalfile <- fromRepo gitAnnexInodeSentinal
	sentinalcachefile <- fromRepo gitAnnexInodeSentinalCache
	liftIO $ writeFile sentinalfile ""
	liftIO $ maybe noop (writeFile sentinalcachefile . showInodeCache)
		=<< genInodeCache sentinalfile

{- The sentinal file is only created when first initializing a repository.
 - If there are any annexed objects in the repository already, creating
 - the file would invalidate their inode caches. -}
createInodeSentinalFile :: Annex ()
createInodeSentinalFile =
	unlessM (alreadyexists <||> hasobjects)
		writeInodeSentinalFile
  where
	alreadyexists = isJust <$> readInodeSentinalFile
	hasobjects = liftIO . doesDirectoryExist =<< fromRepo gitAnnexObjectDir

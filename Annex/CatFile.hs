{- git cat-file interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.CatFile (
	catFile,
	catFileDetails,
	catObject,
	catTree,
	catCommit,
	catObjectDetails,
	withCatFileHandle,
	catObjectMetaData,
	catFileStop,
	catKey,
	catKey',
	catSymLinkTarget,
	catKeyFile,
	catKeyFileHEAD,
	catKeyFileHidden,
	catObjectMetaDataHidden,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.PosixCompat.Types
import Control.Concurrent.STM

import Annex.Common
import qualified Git
import qualified Git.CatFile
import qualified Annex
import Git.Types
import Git.FilePath
import Git.Index
import qualified Git.Ref
import Annex.Link
import Annex.CurrentBranch
import Types.AdjustedBranch
import Types.CatFileHandles
import Utility.ResourcePool

catFile :: Git.Branch -> OsPath -> Annex L.ByteString
catFile branch file = withCatFileHandle $ \h -> 
	liftIO $ Git.CatFile.catFile h branch file

catFileDetails :: Git.Branch -> OsPath -> Annex (Maybe (L.ByteString, Sha, ObjectType))
catFileDetails branch file = withCatFileHandle $ \h -> 
	liftIO $ Git.CatFile.catFileDetails h branch file

catObject :: Git.Ref -> Annex L.ByteString
catObject ref = withCatFileHandle $ \h ->
	liftIO $ Git.CatFile.catObject h ref

catObjectMetaData :: Git.Ref -> Annex (Maybe (Sha, Integer, ObjectType))
catObjectMetaData ref = withCatFileMetaDataHandle $ \h ->
	liftIO $ Git.CatFile.catObjectMetaData h ref

catTree :: Git.Ref -> Annex [(FilePath, FileMode)]
catTree ref = withCatFileHandle $ \h -> 
	liftIO $ Git.CatFile.catTree h ref

catCommit :: Git.Ref -> Annex (Maybe Commit)
catCommit ref = withCatFileHandle $ \h -> 
	liftIO $ Git.CatFile.catCommit h ref

catObjectDetails :: Git.Ref -> Annex (Maybe (L.ByteString, Sha, ObjectType))
catObjectDetails ref = withCatFileHandle $ \h ->
	liftIO $ Git.CatFile.catObjectDetails h ref

{- There can be multiple index files, and a different cat-file is needed
 - for each. That is selected by setting GIT_INDEX_FILE in the gitEnv
 - before running this. -}
withCatFileHandle :: (Git.CatFile.CatFileHandle -> Annex a) -> Annex a
withCatFileHandle = withCatFileHandle'
	Git.CatFile.catFileStart
	catFileMap
	(\v m -> v { catFileMap = m })

withCatFileMetaDataHandle :: (Git.CatFile.CatFileMetaDataHandle -> Annex a) -> Annex a
withCatFileMetaDataHandle = withCatFileHandle'
	Git.CatFile.catFileMetaDataStart
	catFileMetaDataMap
	(\v m -> v { catFileMetaDataMap = m })

withCatFileHandle'
	:: (Repo -> IO hdl)
	-> (CatMap -> M.Map FilePath (ResourcePool hdl))
	-> (CatMap -> M.Map FilePath (ResourcePool hdl) -> CatMap)
	-> (hdl -> Annex a)
	-> Annex a
withCatFileHandle' startcat get set a = do
	cfh <- Annex.getState Annex.catfilehandles
	indexfile <- fromMaybe "" . maybe Nothing (lookup indexEnv)
		<$> fromRepo gitEnv
	p <- case cfh of
		CatFileHandlesNonConcurrent m -> case M.lookup indexfile (get m) of
			Just p -> return p
			Nothing -> do
				p <- mkResourcePoolNonConcurrent startcatfile
				let !m' = set m (M.insert indexfile p (get m))
				Annex.changeState $ \s -> s
					{ Annex.catfilehandles = CatFileHandlesNonConcurrent m' }
				return p
		CatFileHandlesPool tm -> do
			m <- liftIO $ atomically $ takeTMVar tm
			case M.lookup indexfile (get m) of
				Just p -> do
					liftIO $ atomically $ putTMVar tm m
					return p
				Nothing -> do
					p  <- mkResourcePool maxCatFiles
					let !m' = set m (M.insert indexfile p (get m))
					liftIO $ atomically $ putTMVar tm m'
					return p
	withResourcePool p startcatfile a
  where
	startcatfile = inRepo startcat

{- A lot of git cat-file processes are unlikely to improve concurrency,
 - because a query to them takes only a little bit of CPU, and tends to be
 - bottlenecked on disk. Also, they each open a number of files, so
 - using too many might run out of file handles. So, only start a maximum
 - of 2.
 -
 - Note that each different index file gets its own pool of cat-files;
 - this is the size of each pool. In all, 4 times this many cat-files
 - may end up running.
 -}
maxCatFiles :: Int
maxCatFiles = 2

{- Stops all running cat-files. Should only be run when it's known that
 - nothing is using the handles, eg at shutdown. -}
catFileStop :: Annex ()
catFileStop = do
	cfh <- Annex.getState Annex.catfilehandles
	m <- case cfh of
		CatFileHandlesNonConcurrent m -> do
			Annex.changeState $ \s -> s { Annex.catfilehandles = CatFileHandlesNonConcurrent emptyCatMap }
			return m
		CatFileHandlesPool tm ->
			liftIO $ atomically $ swapTMVar tm emptyCatMap
	liftIO $ forM_ (M.elems (catFileMap m)) $ \p ->
		freeResourcePool p Git.CatFile.catFileStop
	liftIO $ forM_ (M.elems (catFileMetaDataMap m)) $ \p ->
		freeResourcePool p Git.CatFile.catFileMetaDataStop

{- From ref to a symlink or a pointer file, get the key. -}
catKey :: Ref -> Annex (Maybe Key)
catKey ref = catObjectMetaData ref >>= \case
	Just (_, sz, _) -> catKey' ref sz
	Nothing -> return Nothing

catKey' :: Ref -> FileSize -> Annex (Maybe Key)
catKey' ref sz
	-- Avoid catting large files, that cannot be symlinks or
	-- pointer files, which would require buffering their
	-- content in memory, as well as a lot of IO.
	| sz <= fromIntegral maxPointerSz =
		parseLinkTargetOrPointer . L.toStrict <$> catObject ref
catKey' _ _ = return Nothing

{- Gets a symlink target. -}
catSymLinkTarget :: Sha -> Annex OsPath
catSymLinkTarget sha = fromInternalGitPath . toOsPath . L.toStrict <$> get
  where
	-- Avoid buffering the whole file content, which might be large.
	-- 8192 is enough if it really is a symlink.
	get = L.take 8192 <$> catObject sha

{- From a file in the repository back to the key.
 -
 - Ideally, this should reflect the key that's staged in the index,
 - not the key that's committed to HEAD. Unfortunately, git cat-file
 - does not refresh the index file after it's started up, so things
 - newly staged in the index won't show up. It does, however, notice
 - when branches change.
 -
 - For command-line git-annex use, that doesn't matter. It's perfectly
 - reasonable for things staged in the index after the currently running
 - git-annex process to not be noticed by it. However, we do want to see
 - what's in the index, since it may have uncommitted changes not in HEAD
 -
 - For the assistant, this is much more of a problem, since it commits
 - files and then needs to be able to immediately look up their keys.
 - OTOH, the assistant doesn't keep changes staged in the index for very
 - long at all before committing them -- and it won't look at the keys
 - of files until after committing them.
 -
 - So, this gets info from the index, unless running as a daemon.
 -}
catKeyFile :: OsPath -> Annex (Maybe Key)
catKeyFile f = ifM (Annex.getState Annex.daemon)
	( catKeyFileHEAD f
	, maybe (pure Nothing) catKey =<< inRepo (Git.Ref.fileRef f)
	)

catKeyFileHEAD :: OsPath -> Annex (Maybe Key)
catKeyFileHEAD f = maybe (pure Nothing) catKey
	=<< inRepo (Git.Ref.fileFromRef Git.Ref.headRef f)

{- Look in the original branch from whence an adjusted branch is based
 - to find the file. But only when the adjustment hides some files. -}
catKeyFileHidden :: OsPath -> CurrBranch -> Annex (Maybe Key) 
catKeyFileHidden = hiddenCat catKey

catObjectMetaDataHidden :: OsPath -> CurrBranch -> Annex (Maybe (Sha, Integer, ObjectType))
catObjectMetaDataHidden = hiddenCat catObjectMetaData

hiddenCat :: (Ref -> Annex (Maybe a)) -> OsPath -> CurrBranch -> Annex (Maybe a)
hiddenCat a f (Just origbranch, Just adj)
	| adjustmentHidesFiles adj = 
		maybe (pure Nothing) a
			=<< inRepo (Git.Ref.fileFromRef origbranch f)
hiddenCat _ _ _ = return Nothing

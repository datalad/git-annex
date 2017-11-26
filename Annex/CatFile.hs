{- git cat-file interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2011-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.CatFile (
	catFile,
	catFileDetails,
	catObject,
	catTree,
	catCommit,
	catObjectDetails,
	catFileHandle,
	catFileStop,
	catKey,
	catKeyFile,
	catKeyFileHEAD,
	catSymLinkTarget,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.PosixCompat.Types

import Annex.Common
import qualified Git
import qualified Git.CatFile
import qualified Annex
import Git.Types
import Git.FilePath
import Git.Index
import qualified Git.Ref
import Annex.Link
import Utility.FileSystemEncoding

catFile :: Git.Branch -> FilePath -> Annex L.ByteString
catFile branch file = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catFile h branch file

catFileDetails :: Git.Branch -> FilePath -> Annex (Maybe (L.ByteString, Sha, ObjectType))
catFileDetails branch file = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catFileDetails h branch file

catObject :: Git.Ref -> Annex L.ByteString
catObject ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObject h ref

catObjectMetaData :: Git.Ref -> Annex (Maybe (Integer, ObjectType))
catObjectMetaData ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObjectMetaData h ref

catTree :: Git.Ref -> Annex [(FilePath, FileMode)]
catTree ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catTree h ref

catCommit :: Git.Ref -> Annex (Maybe Commit)
catCommit ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catCommit h ref

catObjectDetails :: Git.Ref -> Annex (Maybe (L.ByteString, Sha, ObjectType))
catObjectDetails ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObjectDetails h ref

{- There can be multiple index files, and a different cat-file is needed
 - for each. This is selected by setting GIT_INDEX_FILE in the gitEnv. -}
catFileHandle :: Annex Git.CatFile.CatFileHandle
catFileHandle = do
	m <- Annex.getState Annex.catfilehandles
	indexfile <- fromMaybe "" . maybe Nothing (lookup indexEnv)
		<$> fromRepo gitEnv
	case M.lookup indexfile m of
		Just h -> return h
		Nothing -> do
			h <- inRepo Git.CatFile.catFileStart
			let m' = M.insert indexfile h m
			Annex.changeState $ \s -> s { Annex.catfilehandles = m' }
			return h

{- Stops all running cat-files. Should only be run when it's known that
 - nothing is using the handles, eg at shutdown. -}
catFileStop :: Annex ()
catFileStop = do
	m <- Annex.withState $ pure . \s ->
		(s { Annex.catfilehandles = M.empty }, Annex.catfilehandles s)
	liftIO $ mapM_ Git.CatFile.catFileStop (M.elems m)

{- From ref to a symlink or a pointer file, get the key. -}
catKey :: Ref -> Annex (Maybe Key)
catKey ref = go =<< catObjectMetaData ref
  where
	go (Just (sz, _))
		-- Avoid catting large files, that cannot be symlinks or
		-- pointer files, which would require buffering their
		-- content in memory, as well as a lot of IO.
		| sz <= maxPointerSz = parseLinkOrPointer <$> catObject ref
	go _ = return Nothing

{- Gets a symlink target. -}
catSymLinkTarget :: Sha -> Annex String
catSymLinkTarget sha = fromInternalGitPath . decodeBS <$> get
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
catKeyFile :: FilePath -> Annex (Maybe Key)
catKeyFile f = ifM (Annex.getState Annex.daemon)
	( catKeyFileHEAD f
	, catKey $ Git.Ref.fileRef f
	)

catKeyFileHEAD :: FilePath -> Annex (Maybe Key)
catKeyFileHEAD f = catKey $ Git.Ref.fileFromRef Git.Ref.headRef f

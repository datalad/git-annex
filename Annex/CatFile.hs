{- git cat-file interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.CatFile (
	catFile,
	catObject,
	catTree,
	catObjectDetails,
	catFileHandle,
	catKey,
	catKeyFile,
	catKeyFileHEAD,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.PosixCompat.Types

import Common.Annex
import qualified Git
import qualified Git.CatFile
import qualified Annex
import Git.Types
import Git.FilePath
import Git.FileMode
import qualified Git.Ref

catFile :: Git.Branch -> FilePath -> Annex L.ByteString
catFile branch file = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catFile h branch file

catObject :: Git.Ref -> Annex L.ByteString
catObject ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObject h ref

catTree :: Git.Ref -> Annex [(FilePath, FileMode)]
catTree ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catTree h ref

catObjectDetails :: Git.Ref -> Annex (Maybe (L.ByteString, Sha, ObjectType))
catObjectDetails ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObjectDetails h ref

{- There can be multiple index files, and a different cat-file is needed
 - for each. This is selected by setting GIT_INDEX_FILE in the gitEnv. -}
catFileHandle :: Annex Git.CatFile.CatFileHandle
catFileHandle = do
	m <- Annex.getState Annex.catfilehandles
	indexfile <- fromMaybe "" . maybe Nothing (lookup "GIT_INDEX_FILE")
		<$> fromRepo gitEnv
	case M.lookup indexfile m of
		Just h -> return h
		Nothing -> do
			h <- inRepo Git.CatFile.catFileStart
			let m' = M.insert indexfile h m
			Annex.changeState $ \s -> s { Annex.catfilehandles = m' }
			return h

{- From the Sha or Ref of a symlink back to the key.
 -
 - Requires a mode witness, to guarantee that the file is a symlink.
 -}
catKey :: Ref -> FileMode -> Annex (Maybe Key)
catKey = catKey' True

catKey' :: Bool -> Ref -> FileMode -> Annex (Maybe Key)
catKey' modeguaranteed ref mode
	| isSymLink mode = do
		l <- fromInternalGitPath . encodeW8 . L.unpack <$> get
		return $ if isLinkToAnnex l
			then fileKey $ takeFileName l
			else Nothing
	| otherwise = return Nothing
  where
  	-- If the mode is not guaranteed to be correct, avoid
	-- buffering the whole file content, which might be large.
	-- 8192 is enough if it really is a symlink.
  	get
		| modeguaranteed = catObject ref
		| otherwise = L.take 8192 <$> catObject ref

{- Looks up the file mode corresponding to the Ref using the running
 - cat-file.
 -
 - Currently this always has to look in HEAD, because cat-file --batch
 - does not offer a way to specify that we want to look up a tree object
 - in the index. So if the index has a file staged not as a symlink,
 - and it is a symlink in head, the wrong mode is gotten.
 - Also, we have to assume the file is a symlink if it's not yet committed
 - to HEAD. For these reasons, modeguaranteed is not set.
 -}
catKeyChecked :: Bool -> Ref -> Annex (Maybe Key)
catKeyChecked needhead ref@(Ref r) =
	catKey' False ref =<< findmode <$> catTree treeref
  where
  	pathparts = split "/" r
	dir = intercalate "/" $ take (length pathparts - 1) pathparts
	file = fromMaybe "" $ lastMaybe pathparts
	treeref = Ref $ if needhead then "HEAD" ++ dir ++ "/" else dir ++ "/"
	findmode = fromMaybe symLinkMode . headMaybe .
		 map snd . filter (\p -> fst p == file)

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
 - what's in the index, since it may have uncommitted changes not in HEAD>
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
	, catKeyChecked True $ Git.Ref.fileRef f
	)

catKeyFileHEAD :: FilePath -> Annex (Maybe Key)
catKeyFileHEAD f = catKeyChecked False $ Git.Ref.fileFromRef Git.Ref.headRef f

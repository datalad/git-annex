{- git-annex direct mode
 -
 - This only contains some remnants needed to convert away from direct mode.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V5.Direct (
	switchHEADBack,
	setIndirect,
	goodContent,
	associatedFiles,
	removeAssociatedFiles,
	removeInodeCache,
) where

import Annex.Common
import qualified Git
import qualified Git.Config
import qualified Git.Ref
import qualified Git.Branch
import Git.Types
import Config
import Annex.Perms
import Utility.InodeCache
import Annex.InodeSentinal
import qualified Utility.RawFilePath as R

setIndirect :: Annex ()
setIndirect = do
	setbare
	switchHEADBack
	setConfig (annexConfig "direct") val
  where
	val = Git.Config.boolConfig False
	coreworktree = ConfigKey "core.worktree"
	indirectworktree = ConfigKey "core.indirect-worktree"
	setbare = do
		-- core.worktree is not compatible with
		-- core.bare; git does not allow both to be set, so
		-- unset it when enabling direct mode, caching in
		-- core.indirect-worktree
		moveconfig indirectworktree coreworktree
		setConfig Git.Config.coreBare val
	moveconfig src dest = getConfigMaybe src >>= \case
		Nothing -> noop
		Just wt -> do
			unsetConfig src
			setConfig dest (fromConfigValue wt)
			reloadConfig

{- Converts a directBranch back to the original branch.
 -
 - Any other ref is left unchanged.
 -}
fromDirectBranch :: Ref -> Ref
fromDirectBranch directhead = case splitc '/' $ fromRef directhead of
	("refs":"heads":"annex":"direct":rest) -> 
		Ref $ encodeBS $ "refs/heads/" ++ intercalate "/" rest
	_ -> directhead

switchHEADBack :: Annex ()
switchHEADBack = maybe noop switch =<< inRepo Git.Branch.currentUnsafe
  where
	switch currhead = do
		let orighead = fromDirectBranch currhead
		inRepo (Git.Ref.sha currhead) >>= \case
			Just headsha
				| orighead == currhead -> noop
				| otherwise -> do
					inRepo $ Git.Branch.update "leaving direct mode" orighead headsha
					inRepo $ Git.Branch.checkout orighead
					inRepo $ Git.Branch.delete currhead
			Nothing -> inRepo $ Git.Branch.checkout orighead

{- Absolute FilePaths of Files in the tree that are associated with a key. -}
associatedFiles :: Key -> Annex [FilePath]
associatedFiles key = do
	files <- associatedFilesRelative key
	top <- fromRawFilePath <$> fromRepo Git.repoPath
	return $ map (top </>) files

{- List of files in the tree that are associated with a key, relative to
 - the top of the repo. -}
associatedFilesRelative :: Key -> Annex [FilePath] 
associatedFilesRelative key = do
	mapping <- fromRawFilePath <$> calcRepo (gitAnnexMapping key)
	liftIO $ catchDefaultIO [] $ withFile mapping ReadMode $ \h ->
		-- Read strictly to ensure the file is closed promptly
		lines <$> hGetContentsStrict h

{- Removes the list of associated files. -}
removeAssociatedFiles :: Key -> Annex ()
removeAssociatedFiles key = do
	mapping <- calcRepo $ gitAnnexMapping key
	modifyContentDir mapping $
		liftIO $ removeWhenExistsWith R.removeLink mapping

{- Checks if a file in the tree, associated with a key, has not been modified.
 -
 - To avoid needing to fsck the file's content, which can involve an
 - expensive checksum, this relies on a cache that contains the file's
 - expected mtime and inode.
 -}
goodContent :: Key -> FilePath -> Annex Bool
goodContent key file =
	sameInodeCache (toRawFilePath file)
		=<< recordedInodeCache key

{- Gets the recorded inode cache for a key. 
 -
 - A key can be associated with multiple files, so may return more than
 - one. -}
recordedInodeCache :: Key -> Annex [InodeCache]
recordedInodeCache key = withInodeCacheFile key $ \f ->
	liftIO $ catchDefaultIO [] $
		mapMaybe readInodeCache . lines
			<$> readFileStrict (fromRawFilePath f)

{- Removes an inode cache. -}
removeInodeCache :: Key -> Annex ()
removeInodeCache key = withInodeCacheFile key $ \f ->
	modifyContentDir f $
		liftIO $ removeWhenExistsWith R.removeLink f

withInodeCacheFile :: Key -> (RawFilePath -> Annex a) -> Annex a
withInodeCacheFile key a = a =<< calcRepo (gitAnnexInodeCache key)

{- File that maps from a key to the file(s) in the git repository. -}
gitAnnexMapping :: Key -> Git.Repo -> GitConfig -> IO RawFilePath
gitAnnexMapping key r c = do
	loc <- gitAnnexLocation key r c
	return $ loc <> ".map"

{- File that caches information about a key's content, used to determine
 - if a file has changed. -}
gitAnnexInodeCache :: Key -> Git.Repo -> GitConfig -> IO RawFilePath
gitAnnexInodeCache key r c = do
	loc <- gitAnnexLocation key r c
	return $ loc <> ".cache"

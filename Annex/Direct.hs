{- git-annex direct mode
 -
 - This only contains some remnants needed to convert away from direct mode.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Direct (
	switchHEADBack,
	stageDirect,
	setIndirect,
) where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.Config
import qualified Git.Ref
import qualified Git.Branch
import Git.Types
import Config
import Annex.CatFile
import qualified Annex.Queue
import Logs.Location
import Backend
import Types.KeySource
import Annex.Content.Direct
import Annex.Link
import Utility.InodeCache
import Annex.InodeSentinal
import Utility.Metered

{- Uses git ls-files to find files that need to be committed, and stages
 - them into the index. Returns True if some changes were staged. -}
stageDirect :: Annex Bool
stageDirect = do
	Annex.Queue.flush
	top <- fromRepo Git.repoPath
	(l, cleanup) <- inRepo $ Git.LsFiles.stagedOthersDetails [top]
	forM_ l go
	void $ liftIO cleanup
	staged <- Annex.Queue.size
	Annex.Queue.flush
	return $ staged /= 0
  where
	{- Determine what kind of modified or deleted file this is, as
	 - efficiently as we can, by getting any key that's associated
	 - with it in git, as well as its stat info. -}
	go (file, Just sha, Just _mode) = withTSDelta $ \delta -> do
		shakey <- catKey sha
		mstat <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		mcache <- liftIO $ maybe (pure Nothing) (toInodeCache delta file) mstat
		filekey <- isAnnexLink file >>= \case
			Just k -> return (Just k)
			-- v7 unlocked pointer file
			Nothing -> liftIO (isPointerFile file)
		case (shakey, filekey, mstat, mcache) of
			(_, Just key, _, _)
				| shakey == filekey -> noop
				{- A changed symlink. -}
				| otherwise -> stageannexlink file key
			(Just key, _, _, Just cache) -> do
				{- All direct mode files will show as
				 - modified, so compare the cache to see if
				 - it really was. -}
				oldcache <- recordedInodeCache key
				case oldcache of
					[] -> modifiedannexed file key cache
					_ -> unlessM (elemInodeCaches cache oldcache) $
						modifiedannexed file key cache
			(Just key, _, Nothing, _) -> deletedannexed file key
			(Nothing, _, Nothing, _) -> deletegit file
			(_, _, Just _, _) -> addgit file
	go _ = noop

	modifiedannexed file oldkey cache = do
		void $ removeAssociatedFile oldkey file
		void $ addDirect file cache
	
	deletedannexed file key = do
		void $ removeAssociatedFile key file
		deletegit file
	
	stageannexlink file key = do
		l <- calcRepo $ gitAnnexLink file key
		stageSymlink file =<< hashSymlink l
		void $ addAssociatedFile key file

	addgit file = Annex.Queue.addCommand "add" [Param "-f"] [file]

	deletegit file = Annex.Queue.addCommand "rm" [Param "-qf"] [file]

{- Adds a file to the annex in direct mode. Can fail, if the file is
 - modified or deleted while it's being added. -}
addDirect :: FilePath -> InodeCache -> Annex Bool
addDirect file cache = do
	showStart "add" file
	let source = KeySource
		{ keyFilename = file
		, contentLocation = file
		, inodeCache = Just cache
		}
	got =<< genKey source nullMeterUpdate=<< chooseBackend file
  where
	got Nothing = do
		showEndFail
		return False
	got (Just (key, _)) = ifM (sameInodeCache file [cache])
		( do
			l <- calcRepo $ gitAnnexLink file key
			stageSymlink file =<< hashSymlink l
			addInodeCache key cache
			void $ addAssociatedFile key file
			logStatus key InfoPresent
			showEndOk
			return True
		, do
			showEndFail
			return False
		)

setIndirect :: Annex ()
setIndirect = do
	setbare
	switchHEADBack
	setConfig (annexConfig "direct") val
	Annex.changeGitConfig $ \c -> c { annexDirect = False }
  where
	val = Git.Config.boolConfig False
	coreworktree = ConfigKey "core.worktree"
	indirectworktree = ConfigKey "core.indirect-worktree"
	setbare = do
		-- core.worktree is not compatable with
		-- core.bare; git does not allow both to be set, so
		-- unset it when enabling direct mode, caching in
		-- core.indirect-worktree
		moveconfig indirectworktree coreworktree
		setConfig (ConfigKey Git.Config.coreBare) val
	moveconfig src dest = getConfigMaybe src >>= \case
		Nothing -> noop
		Just wt -> do
			unsetConfig src
			setConfig dest wt
			reloadConfig

{- Converts a directBranch back to the original branch.
 -
 - Any other ref is left unchanged.
 -}
fromDirectBranch :: Ref -> Ref
fromDirectBranch directhead = case splitc '/' $ fromRef directhead of
	("refs":"heads":"annex":"direct":rest) -> 
		Ref $ "refs/heads/" ++ intercalate "/" rest
	_ -> directhead

switchHEADBack :: Annex ()
switchHEADBack = maybe noop switch =<< inRepo Git.Branch.currentUnsafe
  where
	switch currhead = do
		let orighead = fromDirectBranch currhead
		inRepo (Git.Ref.sha currhead) >>= \case
			Just headsha
				| orighead /= currhead -> do
					inRepo $ Git.Branch.update "leaving direct mode" orighead headsha
					inRepo $ Git.Branch.checkout orighead
					inRepo $ Git.Branch.delete currhead
			_ -> inRepo $ Git.Branch.checkout orighead

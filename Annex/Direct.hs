{- git-annex direct mode
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Direct where

import Common.Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Annex.Queue
import Git.Types
import Annex.CatFile
import Logs.Location
import Backend
import Types.KeySource
import Annex.Content
import Annex.Content.Direct

{- Uses git ls-files to find files that need to be committed, and stages
 - them into the index. Returns True if some changes were staged. -}
stageDirect :: Annex Bool
stageDirect = do
	Annex.Queue.flush
	top <- fromRepo Git.repoPath
	(l, cleanup) <- inRepo $ Git.LsFiles.stagedDetails [top]
	forM_ l go
	void $ liftIO cleanup
	staged <- Annex.Queue.size
	Annex.Queue.flush
	return $ staged /= 0
  where
	{- Determine what kind of modified or deleted file this is, as
	 - efficiently as we can, by getting any key that's associated
	 - with it in git, as well as its stat info. -}
	go (file, Just sha) = do
		mkey <- catKey sha
		mstat <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case (mkey, mstat, toCache =<< mstat) of
			(Just key, _, Just cache) -> do
				{- All direct mode files will show as
				 - modified, so compare the cache to see if
				 - it really was. -}
				oldcache <- recordedCache key
				when (oldcache /= Just cache) $
					modifiedannexed file key cache
			(Just key, Nothing, _) -> deletedannexed file key
			(Nothing, Nothing, _) -> deletegit file
			(_, Just _, _) -> addgit file
	go (file, Nothing) = do
		mstat <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case (mstat, toCache =<< mstat) of
			(Nothing, _) -> noop
			(Just stat, Just cache)
				| isSymbolicLink stat -> addgit file
				| otherwise -> void $ addDirect file cache
			(Just stat, Nothing)
				| isSymbolicLink stat -> addgit file
				| otherwise -> noop

	modifiedannexed file oldkey cache = do
		void $ removeAssociatedFile oldkey file
		void $ addDirect file cache
	
	deletedannexed file key = do
		void $ removeAssociatedFile key file
		deletegit file
	
	addgit file = Annex.Queue.addCommand "add" [Param "-f"] [file]

	deletegit file = Annex.Queue.addCommand "rm" [Param "-f"] [file]

{- Adds a file to the annex in direct mode. Can fail, if the file is
 - modified or deleted while it's being added. -}
addDirect :: FilePath -> Cache -> Annex Bool
addDirect file cache = do
	showStart "add" file
	let source = KeySource
		{ keyFilename = file
		, contentLocation = file
		}
	got =<< genKey source =<< chooseBackend file
  where
	got Nothing = do
		showEndFail
		return False
	got (Just (key, _)) = ifM (compareCache file $ Just cache)
		( do
			link <- calcGitLink file key
			sha <- inRepo $ Git.HashObject.hashObject BlobObject link
			Annex.Queue.addUpdateIndex =<<
				inRepo (Git.UpdateIndex.stageSymlink file sha)
			writeCache key cache
			void $ addAssociatedFile key file
			logStatus key InfoPresent
			showEndOk
			return True
		, do
			showEndFail
			return False
		)

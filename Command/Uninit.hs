{- git-annex command
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Uninit where

import Command
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Command.Unannex
import qualified Annex.Branch
import qualified Annex.Queue
import qualified Database.Keys
import Annex.Content
import Annex.Init
import Annex.CheckIgnore
import Annex.WorkTree
import Utility.FileMode
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (linkCount)

cmd :: Command
cmd = addCheck check $ 
	command "uninit" SectionUtility
		"de-initialize git-annex and clean out repository"
		paramNothing (withParams seek)

check :: Annex ()
check = do
	b <- current_branch
	when (b == Just Annex.Branch.name) $ giveup $
		"cannot uninit when the " ++ Git.fromRef Annex.Branch.name ++ " branch is checked out"
	top <- fromRepo Git.repoPath
	currdir <- liftIO R.getCurrentDirectory
	whenM ((/=) <$> liftIO (absPath top) <*> liftIO (absPath currdir)) $
		giveup "can only run uninit from the top of the git repository"
  where
	current_branch = 
		ifM (inRepo Git.Ref.headExists)
			( Just . Git.Ref . encodeBS . Prelude.head . lines . decodeBS <$> revhead
			, return Nothing
			)
	revhead = inRepo $ Git.Command.pipeReadStrict
		[Param "rev-parse", Param "--abbrev-ref", Param "HEAD"]

seek :: CommandParams -> CommandSeek
seek = withNothing $ do
	withFilesNotInGit
		(CheckGitIgnore False)
		(WarnUnmatchWorkTreeItems "uninit")
		checksymlinks
		=<< workTreeItems ww []
	withFilesInGitAnnex ww (Command.Unannex.seeker True)
		=<< workTreeItems ww []
	finish
  where
	ww = WarnUnmatchLsFiles "uninit"
	checksymlinks (_, f) = 
		commandAction $ lookupKey f >>= \case
			Nothing -> stop
			Just k -> startCheckIncomplete (fromRawFilePath f) k

{- git annex symlinks that are not checked into git could be left by an
 - interrupted add. -}
startCheckIncomplete :: FilePath -> Key -> CommandStart
startCheckIncomplete file _ = giveup $ unlines
	[ file ++ " points to annexed content, but is not checked into git."
	, "Perhaps this was left behind by an interrupted git annex add?"
	, "Not continuing with uninit; either delete or git annex add the file and retry."
	]

finish :: Annex ()
finish = do
	Annex.Queue.flush
	annexdir <- fromRawFilePath <$> fromRepo gitAnnexDir
	annexobjectdir <- fromRepo gitAnnexObjectDir
	leftovers <- removeUnannexed =<< listKeys InAnnex
	prepareRemoveAnnexDir annexdir
	if null leftovers
		then liftIO $ removeDirectoryRecursive annexdir
		else giveup $ unlines
			[ "Not fully uninitialized"
			, "Some annexed data is still left in " ++ fromRawFilePath annexobjectdir
			, "This may include deleted files, or old versions of modified files."
			, ""
			, "If you don't care about preserving the data, just delete the"
			, "directory."
			, ""
			, "Or, you can move it to another location, in case it turns out"
			, "something in there is important."
			, ""
			, "Or, you can run `git annex unused` followed by `git annex dropunused`"
			, "to remove data that is not used by any tag or branch, which might"
			, "take care of all the data."
			, ""
			, "Then run `git annex uninit` again to finish."
			]
	uninitialize
	-- avoid normal shutdown
	saveState False
	whenM (inRepo $ Git.Ref.exists Annex.Branch.fullname) $
		inRepo $ Git.Command.run
			[Param "branch", Param "-D", Param $ Git.fromRef Annex.Branch.name]
	liftIO exitSuccess

{- Turn on write bits in all remaining files in the annex directory, in
 - preparation for removal. 
 -
 - Also closes sqlite databases that might be in the directory,
 - to avoid later failure to write any cached changes to them. -}
prepareRemoveAnnexDir :: FilePath -> Annex ()
prepareRemoveAnnexDir annexdir = do
	Database.Keys.closeDb
	liftIO $ prepareRemoveAnnexDir' annexdir

prepareRemoveAnnexDir' :: FilePath -> IO ()
prepareRemoveAnnexDir' annexdir =
	dirTreeRecursiveSkipping (const False) annexdir 
		>>= mapM_ (void . tryIO . allowWrite . toRawFilePath)

{- Keys that were moved out of the annex have a hard link still in the
 - annex, with > 1 link count, and those can be removed.
 -
 - Returns keys that cannot be removed. -}
removeUnannexed :: [Key] -> Annex [Key]
removeUnannexed = go []
  where
	go c [] = return c
	go c (k:ks) = ifM (inAnnexCheck k $ liftIO . enoughlinks)
		( do
			lockContentForRemoval k noop removeAnnex
			go c ks
		, go (k:c) ks
		)
	enoughlinks f = catchBoolIO $ do
		s <- R.getFileStatus f
		return $ linkCount s > 1

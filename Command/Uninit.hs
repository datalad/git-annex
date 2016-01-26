{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

import Command
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Command.Unannex
import qualified Annex.Branch
import Annex.Content
import Annex.Init
import Utility.FileMode

import System.IO.HVFS
import System.IO.HVFS.Utils

cmd :: Command
cmd = addCheck check $ 
	command "uninit" SectionUtility
		"de-initialize git-annex and clean out repository"
		paramPaths (withParams seek)

check :: Annex ()
check = do
	b <- current_branch
	when (b == Annex.Branch.name) $ error $
		"cannot uninit when the " ++ Git.fromRef b ++ " branch is checked out"
	top <- fromRepo Git.repoPath
	currdir <- liftIO getCurrentDirectory
	whenM ((/=) <$> liftIO (absPath top) <*> liftIO (absPath currdir)) $
		error "can only run uninit from the top of the git repository"
  where
	current_branch = Git.Ref . Prelude.head . lines <$> revhead
	revhead = inRepo $ Git.Command.pipeReadStrict
		[Param "rev-parse", Param "--abbrev-ref", Param "HEAD"]

seek :: CmdParams -> CommandSeek
seek ps = do
	withFilesNotInGit False (whenAnnexed startCheckIncomplete) ps
	Annex.changeState $ \s -> s { Annex.fast = True }
	withFilesInGit (whenAnnexed Command.Unannex.start) ps
	finish

{- git annex symlinks that are not checked into git could be left by an
 - interrupted add. -}
startCheckIncomplete :: FilePath -> Key -> CommandStart
startCheckIncomplete file _ = error $ unlines
	[ file ++ " points to annexed content, but is not checked into git."
	, "Perhaps this was left behind by an interrupted git annex add?"
	, "Not continuing with uninit; either delete or git annex add the file and retry."
	]

finish :: Annex ()
finish = do
	annexdir <- fromRepo gitAnnexDir
	annexobjectdir <- fromRepo gitAnnexObjectDir
	leftovers <- removeUnannexed =<< getKeysPresent InAnnex
	liftIO $ prepareRemoveAnnexDir annexdir
	if null leftovers
		then liftIO $ removeDirectoryRecursive annexdir
		else error $ unlines
			[ "Not fully uninitialized"
			, "Some annexed data is still left in " ++ annexobjectdir
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
	inRepo $ Git.Command.run
		[Param "branch", Param "-D", Param $ Git.fromRef Annex.Branch.name]
	liftIO exitSuccess

{- Turn on write bits in all remaining files in the annex directory, in
 - preparation for removal. -}
prepareRemoveAnnexDir :: FilePath -> IO ()
prepareRemoveAnnexDir annexdir =
	recurseDir SystemFS annexdir >>= mapM_ (void . tryIO . allowWrite)

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
			lockContentForRemoval k removeAnnex
			go c ks
		, go (k:c) ks
		)
	enoughlinks f = catchBoolIO $ do
		s <- getFileStatus f
		return $ linkCount s > 1

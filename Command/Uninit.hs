{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

import Common.Annex
import Command
import qualified Git
import qualified Git.Command
import qualified Command.Unannex
import qualified Annex.Branch
import Annex.Content
import Annex.Init

def :: [Command]
def = [addCheck check $ command "uninit" paramPaths seek 
	SectionUtility "de-initialize git-annex and clean out repository"]

check :: Annex ()
check = do
	b <- current_branch
	when (b == Annex.Branch.name) $ error $
		"cannot uninit when the " ++ Git.fromRef b ++ " branch is checked out"
	top <- fromRepo Git.repoPath
	cwd <- liftIO getCurrentDirectory
	whenM ((/=) <$> liftIO (absPath top) <*> liftIO (absPath cwd)) $
		error "can only run uninit from the top of the git repository"
  where
	current_branch = Git.Ref . Prelude.head . lines <$> revhead
	revhead = inRepo $ Git.Command.pipeReadStrict
		[Params "rev-parse --abbrev-ref HEAD"]

seek :: CommandSeek
seek ps = do
	withFilesNotInGit (whenAnnexed startCheckIncomplete) ps
	withFilesInGit (whenAnnexed Command.Unannex.start) ps
	finish

{- git annex symlinks that are not checked into git could be left by an
 - interrupted add. -}
startCheckIncomplete :: FilePath -> (Key, Backend) -> CommandStart
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
			removeAnnex k
			go c ks
		, go (k:c) ks
		)
	enoughlinks f = catchBoolIO $ do
		s <- getFileStatus f
		return $ linkCount s > 1

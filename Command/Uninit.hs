{- git-annex command
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Uninit where

import Command
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
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
import Remote.List

import System.PosixCompat.Files (linkCount)
import Control.Concurrent.STM

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "uninit" SectionUtility
		"de-initialize git-annex and clean out repository"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing $ do
	ok <- liftIO $ newTVarIO False
	let checkok v a = do
		liftIO $ atomically $ writeTVar ok v
		() <- a	
		liftIO $ atomically $ readTVar ok
	let recordok = do
		liftIO $ atomically $ writeTVar ok True
		return True
	let recordnotok = liftIO $ atomically $ writeTVar ok False

	whenM (checkok False $ commandAction $ checkCanUninit recordok) $ do
		let symlinksok = checkok True $ withFilesNotInGit
			(CheckGitIgnore False)
			(WarnUnmatchWorkTreeItems "uninit")
			(checksymlinks recordnotok)
			=<< workTreeItems ww []
		whenM symlinksok $ do
			withFilesInGitAnnex ww (Command.Unannex.seeker True)
				=<< workTreeItems ww []
			whenM (checkok False $ commandAction $ removeAnnexDir recordok) $ 
				commandAction completeUnitialize
  where
	ww = WarnUnmatchLsFiles "uninit"
	checksymlinks recordnotok (_, f) = 
		commandAction $ lookupKey f >>= \case
			Nothing -> stop
			Just k -> startCheckIncomplete recordnotok f k

checkCanUninit :: CommandCleanup -> CommandStart
checkCanUninit recordok = 
	starting "uninit check" (ActionItemOther Nothing) (SeekInput []) $ do
		runchecks
		next recordok
  where
	runchecks = do
		b <- current_branch
		when (b == Just Annex.Branch.name) $ giveup $
			"cannot uninit when the " ++ Git.fromRef Annex.Branch.name ++ " branch is checked out"
		top <- fromRepo Git.repoPath
		currdir <- liftIO getCurrentDirectory
		whenM ((/=) <$> liftIO (absPath top) <*> liftIO (absPath currdir)) $
			giveup "can only run uninit from the top of the git repository"
	
	current_branch = 
		ifM (inRepo Git.Ref.headExists)
			( headMaybe . map (Git.Ref . encodeBS) . lines . decodeBS <$> revhead
			, return Nothing
			)
	revhead = inRepo $ Git.Command.pipeReadStrict
		[Param "rev-parse", Param "--abbrev-ref", Param "HEAD"]

{- git annex symlinks that are not checked into git could be left by an
 - interrupted add. -}
startCheckIncomplete :: Annex () -> OsPath -> Key -> CommandStart
startCheckIncomplete recordnotok file key =
	starting "uninit check" (mkActionItem (file, key)) (SeekInput []) $ do
		recordnotok
		giveup $ unlines err
  where
	err =
		[ fromOsPath file ++ " points to annexed content, but is not checked into git."
		, "Perhaps this was left behind by an interrupted git annex add?"
		, "Not continuing with uninit; either delete or git annex add the file and retry."
		]

removeAnnexDir :: CommandCleanup -> CommandStart
removeAnnexDir recordok = do
	Annex.Queue.flush
	annexdir <- fromRepo gitAnnexDir
	annexobjectdir <- fromRepo gitAnnexObjectDir
	starting ("uninit objects") (ActionItemOther Nothing) (SeekInput []) $ do
		leftovers <- removeUnannexed =<< listKeys InAnnex
		prepareRemoveAnnexDir annexdir
		if null leftovers
			then do
				liftIO $ removeDirectoryRecursive annexdir
				next recordok
			else giveup $ unlines
				[ "Not fully uninitialized"
				, "Some annexed data is still left in " ++ fromOsPath annexobjectdir
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

{- Turn on write bits in all remaining files in the annex directory, in
 - preparation for removal. 
 -
 - Also closes sqlite databases that might be in the directory,
 - to avoid later failure to write any cached changes to them. -}
prepareRemoveAnnexDir :: OsPath -> Annex ()
prepareRemoveAnnexDir annexdir = do
	Database.Keys.closeDb
	liftIO $ prepareRemoveAnnexDir' annexdir

prepareRemoveAnnexDir' :: OsPath -> IO ()
prepareRemoveAnnexDir' annexdir =
	emptyWhenDoesNotExist (dirTreeRecursiveSkipping (const False) annexdir)
		>>= mapM_ (void . tryIO . allowWrite)

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
			lockContentForRemoval k noop (removeAnnex remoteList)
			go c ks
		, go (k:c) ks
		)
	enoughlinks f = catchBoolIO $ do
		s <- R.getFileStatus (fromOsPath f)
		return $ linkCount s > 1

completeUnitialize :: CommandStart
completeUnitialize =
	starting ("uninit finish") (ActionItemOther Nothing) (SeekInput []) $ do
		uninitialize
		removeAnnexBranch
		next $ return True

removeAnnexBranch :: Annex ()
removeAnnexBranch = do
	-- avoid normal shutdown commit to the branch
	saveState False
	whenM (inRepo $ Git.Ref.exists Annex.Branch.fullname) $
		inRepo $ Git.Branch.delete Annex.Branch.name

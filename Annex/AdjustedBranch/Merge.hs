{- adjusted branch merging
 -
 - Copyright 2016-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Annex.AdjustedBranch.Merge (
	mergeToAdjustedBranch,
) where

import Annex.Common
import Annex.AdjustedBranch
import qualified Annex
import Git
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import qualified Git.Merge
import Git.Sha
import Annex.CatFile
import Annex.AutoMerge
import Annex.Tmp
import Annex.GitOverlay
import Utility.Tmp.Dir
import Utility.CopyFile
import Utility.Directory.Create

import qualified Data.ByteString as S
import qualified System.FilePath.ByteString as P

{- Update the currently checked out adjusted branch, merging the provided
 - branch into it. Note that the provided branch should be a non-adjusted
 - branch. -}
mergeToAdjustedBranch :: Branch -> (OrigBranch, Adjustment) -> [Git.Merge.MergeConfig] -> Bool -> Git.Branch.CommitMode -> Annex Bool
mergeToAdjustedBranch tomerge (origbranch, adj) mergeconfig canresolvemerge commitmode = catchBoolIO $
	join $ preventCommits go
  where
	adjbranch@(AdjBranch currbranch) = originalToAdjusted origbranch adj
	basis = basisBranch adjbranch

	go commitsprevented =
		ifM (inRepo $ Git.Branch.changed currbranch tomerge)
			( do
				(updatedorig, _) <- propigateAdjustedCommits'
					origbranch adj commitsprevented
				changestomerge updatedorig
			, nochangestomerge
			)

	nochangestomerge = return $ return True

	{- Since the adjusted branch changes files, merging tomerge
	 - directly into it would likely result in unncessary merge
	 - conflicts. To avoid those conflicts, instead merge tomerge into
	 - updatedorig. The result of the merge can the be
	 - adjusted to yield the final adjusted branch.
	 -
	 - In order to do a merge into a ref that is not checked out,
	 - set the work tree to a temp directory, and set GIT_DIR
	 - to another temp directory, in which HEAD contains the
	 - updatedorig sha. GIT_COMMON_DIR is set to point to the real
	 - git directory, and so git can read and write objects from there,
	 - but will use GIT_DIR for HEAD and index.
	 -
	 - (Doing the merge this way also lets it run even though the main
	 - index file is currently locked.)
	 -}
	changestomerge (Just updatedorig) = withOtherTmp $ \othertmpdir -> do
		git_dir <- fromRepo Git.localGitDir
		let git_dir' = fromRawFilePath git_dir
		tmpwt <- fromRepo gitAnnexMergeDir
		withTmpDirIn (fromRawFilePath othertmpdir) "git" $ \tmpgit -> withWorkTreeRelated tmpgit $
			withemptydir git_dir tmpwt $ withWorkTree tmpwt $ do
				liftIO $ writeFile (tmpgit </> "HEAD") (fromRef updatedorig)
				-- Copy in refs and packed-refs, to work
				-- around bug in git 2.13.0, which
				-- causes it not to look in GIT_DIR for refs.
				refs <- liftIO $ dirContentsRecursive $
					git_dir' </> "refs"
				let refs' = (git_dir' </> "packed-refs") : refs
				liftIO $ forM_ refs' $ \src ->
					whenM (doesFileExist src) $ do
						dest <- relPathDirToFile git_dir
							(toRawFilePath src)
						let dest' = toRawFilePath tmpgit P.</> dest
						createDirectoryUnder git_dir
							(P.takeDirectory dest')
						void $ createLinkOrCopy src
							(fromRawFilePath dest')
				-- This reset makes git merge not care
				-- that the work tree is empty; otherwise
				-- it will think that all the files have
				-- been staged for deletion, and sometimes
				-- the merge includes these deletions
				-- (for an unknown reason).
				-- http://thread.gmane.org/gmane.comp.version-control.git/297237
				inRepo $ Git.Command.run [Param "reset", Param "HEAD", Param "--quiet"]
				showAction $ "Merging into " ++ fromRef (Git.Ref.base origbranch)
				merged <- autoMergeFrom' tomerge Nothing mergeconfig commitmode True
					(const $ resolveMerge (Just updatedorig) tomerge True)
				if merged
					then do
						!mergecommit <- liftIO $ extractSha
							<$> S.readFile (tmpgit </> "HEAD")
						-- This is run after the commit lock is dropped.
						return $ postmerge mergecommit
					else return $ return False
	changestomerge Nothing = return $ return False
	
	withemptydir git_dir d a = bracketIO setup cleanup (const a)
	  where
		setup = do
			whenM (doesDirectoryExist d) $
				removeDirectoryRecursive d
			createDirectoryUnder git_dir (toRawFilePath d)
		cleanup _ = removeDirectoryRecursive d

	{- A merge commit has been made between the basisbranch and 
	 - tomerge. Update the basisbranch and origbranch to point
	 - to that commit, adjust it to get the new adjusted branch,
	 - and check it out.
	 -
	 - But, there may be unstaged work tree changes that conflict, 
	 - so the check out is done by making a normal merge of
	 - the new adjusted branch.
	 -}
	postmerge (Just mergecommit) = do
		setBasisBranch basis mergecommit
		inRepo $ Git.Branch.update' origbranch mergecommit
		adjtree <- adjustTree adj (BasisBranch mergecommit)
		adjmergecommit <- commitAdjustedTree adjtree (BasisBranch mergecommit)
		-- Make currbranch be the parent, so that merging
		-- this commit will be a fast-forward.
		adjmergecommitff <- commitAdjustedTree' adjtree (BasisBranch mergecommit) [currbranch]
		showAction "Merging into adjusted branch"
		ifM (autoMergeFrom adjmergecommitff (Just currbranch) mergeconfig commitmode canresolvemerge)
			( reparent adjtree adjmergecommit =<< getcurrentcommit
			, return False
			)
	postmerge Nothing = return False

	-- Now that the merge into the adjusted branch is complete,
	-- take the tree from that merge, and attach it on top of the
	-- adjmergecommit, if it's different.
	reparent adjtree adjmergecommit (Just currentcommit) = do
		if (commitTree currentcommit /= adjtree)
			then do
				cmode <- annexCommitMode <$> Annex.getGitConfig
				c <- inRepo $ Git.Branch.commitTree cmode
					("Merged " ++ fromRef tomerge) [adjmergecommit]
					(commitTree currentcommit)
				inRepo $ Git.Branch.update "updating adjusted branch" currbranch c
				propigateAdjustedCommits origbranch adj
			else inRepo $ Git.Branch.update "updating adjusted branch" currbranch adjmergecommit
		return True
	reparent _ _ Nothing = return False

	getcurrentcommit = inRepo Git.Branch.currentUnsafe >>= \case
		Nothing -> return Nothing
		Just c -> catCommit c

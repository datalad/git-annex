{- git-annex automatic merge conflict resolution
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.AutoMerge (autoMergeFrom) where

import Common.Annex
import qualified Annex.Queue
import Annex.Direct
import Annex.CatFile
import Annex.Link
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Git.UpdateIndex as UpdateIndex
import qualified Git.Merge
import qualified Git.Branch
import qualified Git.Ref
import qualified Git
import Git.Types (BlobType(..))
import Config
import Annex.ReplaceFile
import Git.FileMode
import Annex.VariantFile

import qualified Data.Set as S

{- Merges from a branch into the current branch, with automatic merge
 - conflict resolution. -}
autoMergeFrom :: Git.Ref -> Annex Bool
autoMergeFrom branch = do
	showOutput
	ifM isDirect
		( maybe go godirect =<< inRepo Git.Branch.current
		, go
		)
  where
	go = inRepo (Git.Merge.mergeNonInteractive branch) <||> resolveMerge branch
	godirect currbranch = do
		old <- inRepo $ Git.Ref.sha currbranch
		d <- fromRepo gitAnnexMergeDir
		r <- inRepo (mergeDirect d branch) <||> resolveMerge branch
		new <- inRepo $ Git.Ref.sha currbranch
		case (old, new) of
			(Just oldsha, Just newsha) ->
				mergeDirectCleanup d oldsha newsha
			_ -> noop
		return r

{- Resolves a conflicted merge. It's important that any conflicts be
 - resolved in a way that itself avoids later merge conflicts, since
 - multiple repositories may be doing this concurrently.
 -
 - Only annexed files are resolved; other files are left for the user to
 - handle.
 -
 - This uses the Keys pointed to by the files to construct new
 - filenames. So when both sides modified file foo, 
 - it will be deleted, and replaced with files foo.variant-A and
 - foo.variant-B.
 -
 - On the other hand, when one side deleted foo, and the other modified it,
 - it will be deleted, and the modified version stored as file
 - foo.variant-A (or B).
 -
 - It's also possible that one side has foo as an annexed file, and
 - the other as a directory or non-annexed file. The annexed file
 - is renamed to resolve the merge, and the other object is preserved as-is.
 -
 - In indirect mode, the merge is resolved in the work tree and files
 - staged, to clean up from a conflicted merge that was run in the work
 - tree. In direct mode, the work tree is not touched here; files are 
 - staged to the index, and written to the gitAnnexMergeDir, and later
 - mergeDirectCleanup handles updating the work tree.
 -}
resolveMerge :: Git.Ref -> Annex Bool
resolveMerge branch = do
	top <- fromRepo Git.repoPath
	(fs, cleanup) <- inRepo (LsFiles.unmerged [top])
	mergedfs <- catMaybes <$> mapM (resolveMerge' branch) fs
	let merged = not (null mergedfs)
	void $ liftIO cleanup

	unlessM isDirect $ do
		(deleted, cleanup2) <- inRepo (LsFiles.deleted [top])
		unless (null deleted) $
			Annex.Queue.addCommand "rm" [Params "--quiet -f --"] deleted
		void $ liftIO cleanup2

	when merged $ do
		unlessM isDirect $
			cleanConflictCruft mergedfs top
		Annex.Queue.flush
		void $ inRepo $ Git.Command.runBool
			[ Param "commit"
			, Param "-m"
			, Param "git-annex automatic merge conflict fix"
			]
		showLongNote "Merge conflict was automatically resolved; you may want to examine the result."
	return merged

resolveMerge' :: Git.Ref -> LsFiles.Unmerged -> Annex (Maybe FilePath)
resolveMerge' branch u
	| mergeable LsFiles.valUs && mergeable LsFiles.valThem = do
		kus <- getKey LsFiles.valUs
		kthem <- getKey LsFiles.valThem
		case (kus, kthem) of
			-- Both sides of conflict are annexed files
			(Just keyUs, Just keyThem) -> do
				unstageoldfile
				if keyUs == keyThem
					then makelink keyUs
					else do
						makelink keyUs
						makelink keyThem
				return $ Just file
			-- Our side is annexed, other side is not.
			(Just keyUs, Nothing) -> do
				unstageoldfile
				whenM isDirect $
					stagefromdirectmergedir file
				makelink keyUs
				return $ Just file
			-- Our side is not annexed, other side is.
			(Nothing, Just keyThem) -> do
				unstageoldfile
				makelink keyThem
				return $ Just file
			-- Neither side is annexed; cannot resolve.
			(Nothing, Nothing) -> return Nothing
	| otherwise = return Nothing
  where
	file = LsFiles.unmergedFile u
	mergeable select = select (LsFiles.unmergedBlobType u)
		`elem` [Just SymlinkBlob, Nothing]
	makelink key = do
		let dest = variantFile file key
		l <- inRepo $ gitAnnexLink dest key
		ifM isDirect
			( do
				d <- fromRepo gitAnnexMergeDir
				replaceFile (d </> dest) $ makeAnnexLink l
			, replaceFile dest $ makeAnnexLink l
			)
		stageSymlink dest =<< hashSymlink l
	getKey select = case select (LsFiles.unmergedSha u) of
		Nothing -> return Nothing
		Just sha -> catKey sha symLinkMode

	-- removing the conflicted file from cache clears the conflict
	unstageoldfile = Annex.Queue.addCommand "rm" [Params "--quiet -f --cached --"] [file]

	{- stage an item from the direct mode merge directory, which may
	 - be a directory with arbitrary contents -}
	stagefromdirectmergedir item = Annex.Queue.addUpdateIndex
		=<< fromRepo (UpdateIndex.lsSubTree branch item)

{- git-merge moves conflicting files away to files
 - named something like f~HEAD or f~branch, but the
 - exact name chosen can vary. Once the conflict is resolved,
 - this cruft can be deleted. To avoid deleting legitimate
 - files that look like this, only delete files that are
 - A) not staged in git and B) look like git-annex symlinks.
 -}
cleanConflictCruft :: [FilePath] -> FilePath -> Annex ()
cleanConflictCruft resolvedfs top = do
	(fs, cleanup) <- inRepo $ LsFiles.notInRepo False [top]
	mapM_ clean fs
	void $ liftIO cleanup
  where
	clean f
		| matchesresolved f = whenM (isJust <$> isAnnexLink f) $
			liftIO $ nukeFile f
		| otherwise = noop
	s = S.fromList resolvedfs
	matchesresolved f = S.member (base f) s
	base f = reverse $ drop 1 $ dropWhile (/= '~') $ reverse f

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
import qualified Git.Ref
import qualified Git.Sha
import qualified Git
import Git.Types (BlobType(..))
import Config
import Annex.ReplaceFile
import Git.FileMode
import Annex.VariantFile

import qualified Data.Set as S

{- Merges from a branch into the current branch
 - (which may not exist yet),
 - with automatic merge conflict resolution. -}
autoMergeFrom :: Git.Ref -> (Maybe Git.Ref) -> Annex Bool
autoMergeFrom branch currbranch = do
	showOutput
	case currbranch of
		Nothing -> go Nothing
		Just b -> go =<< inRepo (Git.Ref.sha b)
  where
	go old = ifM isDirect
		( do
			d <- fromRepo gitAnnexMergeDir
			r <- inRepo (mergeDirect d branch)
				<||> resolveMerge old branch
			mergeDirectCleanup d (fromMaybe Git.Sha.emptyTree old) Git.Ref.headRef
			return r
		, inRepo (Git.Merge.mergeNonInteractive branch)
			<||> resolveMerge old branch
		)

{- Resolves a conflicted merge. It's important that any conflicts be
 - resolved in a way that itself avoids later merge conflicts, since
 - multiple repositories may be doing this concurrently.
 -
 - Only merge conflicts where at least one side is an annexed file
 - is resolved.
 -
 - This uses the Keys pointed to by the files to construct new
 - filenames. So when both sides modified annexed file foo, 
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
resolveMerge :: Maybe Git.Ref -> Git.Ref -> Annex Bool
resolveMerge us them = do
	top <- fromRepo Git.repoPath
	(fs, cleanup) <- inRepo (LsFiles.unmerged [top])
	mergedfs <- catMaybes <$> mapM (resolveMerge' us them) fs
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
		whenM isDirect $
			void preCommitDirect
		void $ inRepo $ Git.Command.runBool
			[ Param "commit"
			, Param "--no-verify"
			, Param "-m"
			, Param "git-annex automatic merge conflict fix"
			]
		showLongNote "Merge conflict was automatically resolved; you may want to examine the result."
	return merged

resolveMerge' :: Maybe Git.Ref -> Git.Ref -> LsFiles.Unmerged -> Annex (Maybe FilePath)
resolveMerge' Nothing _ _ = return Nothing
resolveMerge' (Just us) them u = do
	kus <- getkey LsFiles.valUs LsFiles.valUs 
	kthem <- getkey LsFiles.valThem LsFiles.valThem
	case (kus, kthem) of
		-- Both sides of conflict are annexed files
		(Just keyUs, Just keyThem)
			| keyUs /= keyThem -> resolveby $ do
				makelink keyUs
				makelink keyThem
			| otherwise -> resolveby $
				makelink keyUs
		-- Our side is annexed file, other side is not.
		(Just keyUs, Nothing) -> resolveby $ do
			graftin them file
			makelink keyUs
		-- Our side is not annexed file, other side is.
		(Nothing, Just keyThem) -> resolveby $ do
			graftin us file
			makelink keyThem
		-- Neither side is annexed file; cannot resolve.
		(Nothing, Nothing) -> return Nothing
  where
	file = LsFiles.unmergedFile u

	getkey select select'
		| select (LsFiles.unmergedBlobType u) == Just SymlinkBlob =
			case select' (LsFiles.unmergedSha u) of
				Nothing -> return Nothing
				Just sha -> catKey sha symLinkMode
		| otherwise = return Nothing
	
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

	{- stage a graft of a directory or file from a branch -}
	graftin b item = Annex.Queue.addUpdateIndex
		=<< fromRepo (UpdateIndex.lsSubTree b item)
		
	resolveby a = do
		{- Remove conflicted file from index so merge can be resolved. -}
		Annex.Queue.addCommand "rm" [Params "--quiet -f --cached --"] [file]
		void a
		return (Just file)

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

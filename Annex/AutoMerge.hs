{- git-annex automatic merge conflict resolution
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.AutoMerge
	( autoMergeFrom
	, autoMergeFrom'
	, resolveMerge
	, commitResolvedMerge
	) where

import Annex.Common
import qualified Annex
import qualified Annex.Queue
import Annex.CatFile
import Annex.Link
import Annex.Content
import qualified Git.LsFiles as LsFiles
import qualified Git.UpdateIndex as UpdateIndex
import qualified Git.Merge
import qualified Git.Ref
import qualified Git
import qualified Git.Branch
import Git.Types (TreeItemType(..), fromTreeItemType)
import Git.FilePath
import Annex.ReplaceFile
import Annex.VariantFile
import qualified Database.Keys
import Annex.InodeSentinal
import Utility.InodeCache
import Utility.FileMode

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Utility.RawFilePath as R

{- Merges from a branch into the current branch (which may not exist yet),
 - with automatic merge conflict resolution.
 -
 - Callers should use Git.Branch.changed first, to make sure that
 - there are changes from the current branch to the branch being merged in.
 -}
autoMergeFrom :: Git.Ref -> Maybe Git.Ref -> [Git.Merge.MergeConfig] -> Git.Branch.CommitMode -> Bool -> Annex Bool
autoMergeFrom branch currbranch mergeconfig commitmode canresolvemerge =
	autoMergeFrom' branch currbranch mergeconfig commitmode canresolvemerge resolvemerge
  where
	resolvemerge old
		| canresolvemerge = resolveMerge old branch False
		| otherwise = return False 

autoMergeFrom' :: Git.Ref -> Maybe Git.Ref -> [Git.Merge.MergeConfig] -> Git.Branch.CommitMode -> Bool -> (Maybe Git.Ref -> Annex Bool) -> Annex Bool
autoMergeFrom' branch currbranch mergeconfig commitmode willresolvemerge toresolvemerge = do
	showOutput
	case currbranch of
		Nothing -> go Nothing
		Just b -> go =<< inRepo (Git.Ref.sha b)
  where
	go old = do
			-- merge.directoryRenames=conflict plus automatic
			-- merge conflict resolution results in files in a
			-- "renamed" directory getting variant names,
			-- so is not a great combination. If the user has
			-- explicitly set it, use it, but otherwise when
			-- merge conflicts will be resolved, override
			-- to merge.directoryRenames=false.
			overridedirectoryrenames <- if willresolvemerge
				then isNothing . mergeDirectoryRenames
					<$> Annex.getGitConfig
				else pure False
			let f r 
				| overridedirectoryrenames = r
					{ Git.gitGlobalOpts = 
						Param "-c"
						: Param "merge.directoryRenames=false" 
						: Git.gitGlobalOpts r
					}
				| otherwise = r
			r <- inRepo (Git.Merge.merge branch mergeconfig commitmode . f)
				<||> (toresolvemerge old <&&> commitResolvedMerge commitmode)
			-- Merging can cause new associated files to appear
			-- and the smudge filter will add them to the database.
			-- To ensure that this process sees those changes,
			-- close the database if it was open.
			Database.Keys.closeDb
			return r

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
 - The merge is resolved in the work tree and files
 - staged, to clean up from a conflicted merge that was run in the work
 - tree. 
 - 
 - This is complicated by needing to support merges run in an overlay
 - work tree, in which case the CWD won't be within the work tree.
 - In this mode, there is no need to update the work tree at all,
 - as the overlay work tree will get deleted.
 -
 - Unlocked files remain unlocked after merging, and locked files
 - remain locked. When the merge conflict is between a locked and unlocked
 - file, that otherwise point to the same content, the unlocked mode wins.
 - This is done because only unlocked files work in filesystems that don't
 - support symlinks.
 -
 - Returns false when there are no merge conflicts to resolve.
 - A git merge can fail for other reasons, and this allows detecting
 - such failures.
 -}
resolveMerge :: Maybe Git.Ref -> Git.Ref -> Bool -> Annex Bool
resolveMerge us them inoverlay = do
	top <- if inoverlay
		then pure "."
		else fromRepo Git.repoPath
	(fs, cleanup) <- inRepo (LsFiles.unmerged [top])
	srcmap <- if inoverlay
		then pure M.empty
		else inodeMap $ pure (map LsFiles.unmergedFile fs, return True)
	(mergedks, mergedfs) <- unzip <$> mapM (resolveMerge' srcmap us them inoverlay) fs
	let mergedks' = concat mergedks
	let mergedfs' = catMaybes mergedfs
	let merged = not (null mergedfs')
	void $ liftIO cleanup

	unless inoverlay $ do
		(deleted, cleanup2) <- inRepo (LsFiles.deleted [] [top])
		unless (null deleted) $
			Annex.Queue.addCommand "rm"
				[Param "--quiet", Param "-f", Param "--"]
				(map fromRawFilePath deleted)
		void $ liftIO cleanup2

	when merged $ do
		Annex.Queue.flush
		unless inoverlay $ do
			unstagedmap <- inodeMap $ inRepo $
				LsFiles.notInRepo [] False [top]
			cleanConflictCruft mergedks' mergedfs' unstagedmap
		showLongNote "Merge conflict was automatically resolved; you may want to examine the result."
	return merged

resolveMerge' :: InodeMap -> Maybe Git.Ref -> Git.Ref -> Bool -> LsFiles.Unmerged -> Annex ([Key], Maybe FilePath)
resolveMerge' _ Nothing _ _ _ = return ([], Nothing)
resolveMerge' unstagedmap (Just us) them inoverlay u = do
	kus <- getkey LsFiles.valUs
	kthem <- getkey LsFiles.valThem
	case (kus, kthem) of
		-- Both sides of conflict are annexed files
		(Just keyUs, Just keyThem)
			| keyUs /= keyThem -> resolveby [keyUs, keyThem] $ do
				makevariantannexlink keyUs LsFiles.valUs
				makevariantannexlink keyThem LsFiles.valThem
				-- cleanConflictCruft can't handle unlocked
				-- files, so delete here.
				unless inoverlay $
					unless (islocked LsFiles.valUs) $
						liftIO $ removeWhenExistsWith removeLink file
			| otherwise -> do
				-- Only resolve using symlink when both
				-- were locked, otherwise use unlocked
				-- pointer.
				-- In either case, keep original filename.
				if islocked LsFiles.valUs && islocked LsFiles.valThem
					then makesymlink keyUs file
					else makepointer keyUs file (combinedmodes)
				return ([keyUs, keyThem], Just file)
		-- Our side is annexed file, other side is not.
		-- Make the annexed file into a variant file and graft in the
		-- other file/directory as it was.
		(Just keyUs, Nothing) -> resolveby [keyUs] $ do
			graftin them file LsFiles.valThem LsFiles.valThem LsFiles.valUs
			makevariantannexlink keyUs LsFiles.valUs 
		-- Our side is not annexed file, other side is.
		(Nothing, Just keyThem) -> resolveby [keyThem] $ do
			graftin us file LsFiles.valUs LsFiles.valUs LsFiles.valThem
			makevariantannexlink keyThem LsFiles.valThem
		-- Neither side is annexed file; cannot resolve.
		(Nothing, Nothing) -> return ([], Nothing)
  where
	file = fromRawFilePath $ LsFiles.unmergedFile u

	getkey select = 
		case select (LsFiles.unmergedSha u) of
			Just sha -> catKey sha
			Nothing -> pure Nothing
	
	islocked select = select (LsFiles.unmergedTreeItemType u) == Just TreeSymlink

	combinedmodes = case catMaybes [ourmode, theirmode] of
		[] -> Nothing
		l -> Just (combineModes l)
	  where
		ourmode = fromTreeItemType
			<$> LsFiles.valUs (LsFiles.unmergedTreeItemType u)
		theirmode = fromTreeItemType
			<$> LsFiles.valThem (LsFiles.unmergedTreeItemType u)

	makevariantannexlink key select
		| islocked select = makesymlink key dest
		| otherwise = makepointer key dest destmode
	  where
		dest = variantFile file key
		destmode = fromTreeItemType <$> select (LsFiles.unmergedTreeItemType u)

	stagefile :: FilePath -> Annex FilePath
	stagefile f
		| inoverlay = (</> f) . fromRawFilePath <$> fromRepo Git.repoPath
		| otherwise = pure f

	makesymlink key dest = do
		l <- calcRepo $ gitAnnexLink (toRawFilePath dest) key
		unless inoverlay $ replacewithsymlink dest l
		dest' <- toRawFilePath <$> stagefile dest
		stageSymlink dest' =<< hashSymlink l

	replacewithsymlink dest link = replaceWorkTreeFile dest $
		makeGitLink link . toRawFilePath

	makepointer key dest destmode = do
		unless inoverlay $ 
			unlessM (reuseOldFile unstagedmap key file dest) $
				linkFromAnnex key (toRawFilePath dest) destmode >>= \case
					LinkAnnexFailed -> liftIO $
						writePointerFile (toRawFilePath dest) key destmode
					_ -> noop
		dest' <- toRawFilePath <$> stagefile dest
		stagePointerFile dest' destmode =<< hashPointerFile key
		unless inoverlay $
			Database.Keys.addAssociatedFile key
				=<< inRepo (toTopFilePath (toRawFilePath dest))

	{- Stage a graft of a directory or file from a branch
	 - and update the work tree. -}
	graftin b item selectwant selectwant' selectunwant = do
		Annex.Queue.addUpdateIndex
			=<< fromRepo (UpdateIndex.lsSubTree b item)

		-- Update the work tree to reflect the graft.
		unless inoverlay $ case (selectwant (LsFiles.unmergedTreeItemType u), selectunwant (LsFiles.unmergedTreeItemType u)) of
			-- Symlinks are never left in work tree when
			-- there's a conflict with anything else.
			-- So, when grafting in a symlink, we must create it:
			(Just TreeSymlink, _) -> do
				case selectwant' (LsFiles.unmergedSha u) of
					Nothing -> noop
					Just sha -> do
						link <- catSymLinkTarget sha
						replacewithsymlink item link
			-- And when grafting in anything else vs a symlink,
			-- the work tree already contains what we want.
			(_, Just TreeSymlink) -> noop
			_ -> ifM (liftIO $ doesDirectoryExist item)
				-- a conflict between a file and a directory
				-- leaves the directory, so since a directory
				-- is there, it must be what was wanted
				( noop
				-- probably a file with conflict markers is
				-- in the work tree; replace with grafted
				-- file content
				, case selectwant' (LsFiles.unmergedSha u) of
					Nothing -> noop
					Just sha -> replaceWorkTreeFile item $ \tmp -> do
						c <- catObject sha
						liftIO $ L.writeFile tmp c
				)
	
	resolveby ks a = do
		{- Remove conflicted file from index so merge can be resolved. -}
		Annex.Queue.addCommand "rm"
			[Param "--quiet", Param "-f", Param "--cached", Param "--"] [file]
		void a
		return (ks, Just file)

{- git-merge moves conflicting files away to files
 - named something like f~HEAD or f~branch or just f, but the
 - exact name chosen can vary. Once the conflict is resolved,
 - this cruft can be deleted. To avoid deleting legitimate
 - files that look like this, only delete files that are
 - A) not staged in git and
 - B) have a name related to the merged files and
 - C) are pointers to or have the content of keys that were involved
 - in the merge.
 -}
cleanConflictCruft :: [Key] -> [FilePath] -> InodeMap -> Annex ()
cleanConflictCruft resolvedks resolvedfs unstagedmap = do
	is <- S.fromList . map (inodeCacheToKey Strongly) . concat 
		<$> mapM Database.Keys.getInodeCaches resolvedks
	forM_ (M.toList unstagedmap) $ \(i, f) ->
		whenM (matchesresolved is i f) $
			liftIO $ removeWhenExistsWith removeLink f
  where
	fs = S.fromList resolvedfs
	ks = S.fromList resolvedks
	inks = maybe False (flip S.member ks)
	matchesresolved is i f
		| S.member f fs || S.member (conflictCruftBase f) fs = anyM id
			[ pure $ either (const False) (`S.member` is) i
			, inks <$> isAnnexLink (toRawFilePath f)
			, inks <$> liftIO (isPointerFile (toRawFilePath f))
			]
		| otherwise = return False

conflictCruftBase :: FilePath -> FilePath
conflictCruftBase f = reverse $ drop 1 $ dropWhile (/= '~') $ reverse f

{- When possible, reuse an existing file from the srcmap as the
 - content of a worktree file in the resolved merge. It must have the
 - same name as the origfile, or a name that git would use for conflict
 - cruft. And, its inode cache must be a known one for the key. -}
reuseOldFile :: InodeMap -> Key -> FilePath -> FilePath -> Annex Bool
reuseOldFile srcmap key origfile destfile = do
	is <- map (inodeCacheToKey Strongly)
		<$> Database.Keys.getInodeCaches key
	liftIO $ go $ mapMaybe (\i -> M.lookup (Right i) srcmap) is
  where
	go [] = return False
	go (f:fs)
		| f == origfile || conflictCruftBase f == origfile = 
			ifM (doesFileExist f)
				( do
					renameFile f destfile
					return True
				, go fs
				)
		| otherwise = go fs
	
commitResolvedMerge :: Git.Branch.CommitMode -> Annex Bool
commitResolvedMerge commitmode = inRepo $ Git.Branch.commitCommand commitmode
	[ Param "--no-verify"
	, Param "-m"
	, Param "git-annex automatic merge conflict fix"
	]

type InodeMap = M.Map (Either FilePath InodeCacheKey) FilePath

inodeMap :: Annex ([RawFilePath], IO Bool) -> Annex InodeMap
inodeMap getfiles = do
	(fs, cleanup) <- getfiles
	fsis <- forM fs $ \f -> do
		s <- liftIO $ R.getSymbolicLinkStatus f
		let f' = fromRawFilePath f
		if isSymbolicLink s
			then pure $ Just (Left f', f')
			else withTSDelta (\d -> liftIO $ toInodeCache d f' s)
				>>= return . \case
					Just i -> Just (Right (inodeCacheToKey Strongly i), f')
					Nothing -> Nothing
	void $ liftIO cleanup
	return $ M.fromList $ catMaybes fsis

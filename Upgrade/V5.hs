{- git-annex v5 -> v6 upgrade support
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V5 where

import Annex.Common
import Types.Upgrade
import Config
import Config.Smudge
import Annex.InodeSentinal
import Annex.Link
import Annex.CatFile
import Annex.WorkTree
import Annex.UUID
import Logs.Location
import qualified Upgrade.V5.Direct as Direct
import qualified Annex.Content as Content
import qualified Database.Keys
import qualified Git
import qualified Git.LsFiles
import qualified Git.Branch
import Git.FilePath
import Git.FileMode
import Git.Config
import Git.Ref
import Utility.InodeCache
import Annex.AdjustedBranch
import qualified Utility.FileIO as F

upgrade :: Bool -> Annex UpgradeResult
upgrade automatic = flip catchNonAsync onexception $ do
	unless automatic $
		showAction "v5 to v6"
	whenM isDirect convertDirect
	scanAnnexedFiles
	configureSmudgeFilter
	-- Inode sentinal file was only used in direct mode and when
	-- locking down files as they were added. In v6, it's used more
	-- extensively, so make sure it exists, since old repos that didn't
	-- use direct mode may not have created it.
	unlessM isDirect $
		createInodeSentinalFile True
	return UpgradeSuccess
  where
	onexception e = do
		warning $ UnquotedString $ "caught exception: " ++ show e
		return UpgradeFailed

convertDirect :: Annex ()
convertDirect = do
	{- Direct mode makes the same tradeoff of using less disk
	 - space, with less preservation of old versions of files
	 - as does annex.thin. -}
	setConfig (annexConfig "thin") (boolConfig True)
	Direct.setIndirect
	cur <- inRepo Git.Branch.current >>= \case
		Just cur -> return cur
		Nothing -> do
			-- Avoid running pre-commit hook.
			commitForAdjustedBranch [Param "--no-verify"]
			fromMaybe (giveup "Nothing is committed, and a commit failed; unable to proceed.") 
				<$> inRepo Git.Branch.current
	upgradeDirectWorkTree
	removeDirectCruft
	{- Create adjusted branch where all files are unlocked.
	 - This should have the same content for each file as
	 - have been staged in upgradeDirectWorkTree. -}
	AdjBranch b <- adjustBranch (LinkAdjustment UnlockAdjustment) cur
	{- Since the work tree was already set up by
	 - upgradeDirectWorkTree, and contains unlocked file
	 - contents too, don't use git checkout to check out the
	 - adjust branch. Instead, update HEAD manually. -}
	inRepo $ setHeadRef b

{- Walk work tree from top and convert all annex symlinks to pointer files,
 - staging them in the index, and populating the annex objects with
 - hard links (or copies) of the work tree files (when not modified or
 - deleted).
 -}
upgradeDirectWorkTree :: Annex ()
upgradeDirectWorkTree = do
	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.inRepoDetails [] [top]
	forM_ l go
	void $ liftIO clean
  where
	go (f, _sha, mode) | isSymLink mode = do
		-- Cannot use lookupKey here, as we're in between direct
		-- mode and v6.
		mk <- catKeyFile f
		case mk of
			Nothing -> noop
			Just k -> do
				stagePointerFile f Nothing =<< hashPointerFile k
				ifM (isJust <$> getAnnexLinkTarget f)
					( writepointer f k
					, fromdirect f k
					)
				Database.Keys.addAssociatedFile k
					=<< inRepo (toTopFilePath f)
	go _ = noop

	fromdirect f k = ifM (Direct.goodContent k f)
		( do
			-- If linkToAnnex fails for some reason, the work tree
			-- file still has the content; the annex object file
			-- is just not populated with it. Since the work tree
			-- file is recorded as an associated file, things will
			-- still work that way, it's just not ideal.
			ic <- withTSDelta (liftIO . genInodeCache f)
			void $ Content.linkToAnnex k f ic
		, unlessM (Content.inAnnex k) $ do
			-- Worktree file was deleted or modified;
			-- if there are no other copies of the content
			-- then it's been lost.
			locs <- Direct.associatedFiles k
			unlessM (anyM (Direct.goodContent k) locs) $ do
				u <- getUUID
				logChange NoLiveUpdate k u InfoMissing
		)
	
	writepointer f k = liftIO $ do
		removeWhenExistsWith removeFile f
		F.writeFile' f (formatPointer k)

{- Remove all direct mode bookkeeping files. -}
removeDirectCruft :: Annex ()
removeDirectCruft = mapM_ go =<< Content.listKeys Content.InAnywhere
  where
	go k = do
		Direct.removeInodeCache k
		Direct.removeAssociatedFiles k

{- git-annex command
 -
 - Copyright 2017-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.PostReceive where

import Command
import qualified Annex
import Annex.UpdateInstead
import Annex.CurrentBranch
import Command.Sync (mergeLocal, prepMerge, mergeConfig, SyncOptions(..))
import Annex.Proxy
import Remote
import qualified Types.Remote as Remote
import Config
import Git.Types
import Git.Sha
import qualified Git.Ref
import Command.Export (filterExport, getExportCommit, seekExport)
import Command.Sync (syncBranch)

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- This does not need to modify the git-annex branch to update the 
-- work tree, but auto-initialization might change the git-annex branch.
-- Since it would be surprising for a post-receive hook to make such a
-- change, that's prevented by noCommit.
cmd :: Command
cmd = noCommit $
	command "post-receive" SectionPlumbing
		"run by git post-receive hook"
		paramNothing
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = do
	fixPostReceiveHookEnv
	whenM needUpdateInsteadEmulation $
		commandAction updateInsteadEmulation
	proxyExportTree

updateInsteadEmulation :: CommandStart
updateInsteadEmulation = do
	prepMerge
	let o = def { notOnlyAnnexOption = True }
	mc <- mergeConfig False
	mergeLocal mc o =<< getCurrentBranch

proxyExportTree :: CommandSeek
proxyExportTree = do
	rbs <- catMaybes <$> (mapM canexport =<< proxyForRemotes)
	unless (null rbs) $ do
		pushedbranches <- liftIO $ 
			S.fromList . map snd . parseHookInput
				<$> B.hGetContents stdin
		let waspushed b = S.member b pushedbranches
			|| S.member (syncBranch b) pushedbranches
		case filter (waspushed . Git.Ref.branchRef . fst . snd) rbs of
			[] -> return ()
			rbs' -> forM_ rbs' $ \((r, b), _) -> go r b
  where
	canexport r = case remoteAnnexTrackingBranch (Remote.gitconfig r) of
		Nothing -> return Nothing
		Just branch ->
			ifM (isExportSupported r)
				( return (Just ((r, branch), splitRemoteAnnexTrackingBranchSubdir branch))
				, return Nothing
				)
	
	go r b = inRepo (Git.Ref.tree b) >>= \case
		Nothing -> return ()
		Just t -> do
			tree <- filterExport r t
			mtbcommitsha <- getExportCommit r b
			seekExport r tree mtbcommitsha []

parseHookInput :: B.ByteString -> [((Sha, Sha), Ref)]
parseHookInput = mapMaybe parse . B8.lines
  where
	parse l = case B8.words l of
		(oldb:newb:refb:[]) -> do
			old <- extractSha oldb
			new <- extractSha newb
			return ((old, new), Ref refb)
		_ -> Nothing

{- When run by the post-receive hook, the cwd is the .git directory, 
 - and GIT_DIR=. It's not clear why git does this.
 -
 - Fix up from that unusual situation, so that git commands
 - won't try to treat .git as the work tree. -}
fixPostReceiveHookEnv :: Annex ()
fixPostReceiveHookEnv = do
	g <- Annex.gitRepo
	case location g of
		Local { gitdir = ".", worktree = Just "." } ->
			Annex.adjustGitRepo $ \g' -> pure $ g'
				{ location = case location g' of
					loc@(Local {}) -> loc 
						{ worktree = Just ".." }
					loc -> loc
				}
		_ -> noop


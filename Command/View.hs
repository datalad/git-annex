{- git-annex command
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.View where

import Command
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.LsFiles as LsFiles
import Git.FilePath
import Git.Status
import Types.View
import Annex.View
import Logs.View
import Types.AdjustedBranch
import Annex.AdjustedBranch.Name

import qualified System.FilePath.ByteString as P

cmd :: Command
cmd = notBareRepo $
	command "view" SectionMetaData "enter a view branch"
		paramView (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start [] = giveup "Specify metadata to include in view"
start ps = ifM safeToEnterView
	( do
		(view, madj) <- mkView ps
		go view madj =<< currentView
	, giveup "Not safe to enter view."
	)
  where
	ai = ActionItemOther Nothing
	si = SeekInput ps
	go view madj Nothing = starting "view" ai si $
		perform view madj
	go view _ (Just (v, _madj))
		| v == view = stop
		| otherwise = giveup "Already in a view. Use the vfilter and vadd commands to further refine this view."

safeToEnterView :: Annex Bool
safeToEnterView = do
	(l, cleanup) <- inRepo $ getStatus [] []
	case filter dangerous l of
		[] -> liftIO cleanup
		_ -> do
			warning "Your uncommitted changes would be lost when entering a view."
			void $ liftIO cleanup
			return False
  where
	dangerous (StagedUnstaged { staged = Nothing, unstaged = Nothing }) = False
	-- Untracked files will not be affected by entering a view,
	-- so are not dangerous.
	dangerous (StagedUnstaged { staged = Just (Untracked _), unstaged = Nothing }) = False
	dangerous (StagedUnstaged { unstaged = Just (Untracked _), staged = Nothing }) = False
	dangerous (StagedUnstaged { unstaged = Just (Untracked _), staged = Just (Untracked _) }) = False
	-- Staged changes would have their modifications either be 
	-- lost when entering a view, or committed as part of the view.
	-- Either is dangerous because upon leaving the view; the staged
	-- changes would be lost.
	dangerous (StagedUnstaged { staged = Just _ }) = True
	-- Unstaged changes to annexed files would get lost when entering a
	-- view.
	dangerous (StagedUnstaged { unstaged = Just _ }) = True

perform :: View -> Maybe Adjustment -> CommandPerform
perform view madj = do
	showAction "searching"
	next $ checkoutViewBranch view madj applyView

paramView :: String
paramView = paramRepeating "TAG FIELD=GLOB ?TAG FIELD?=GLOB FIELD!=VALUE"

mkView :: [String] -> Annex (View, Maybe Adjustment)
mkView ps = go =<< inRepo Git.Branch.current
  where
	go Nothing = giveup "not on any branch!"
	go (Just b) = case adjustedToOriginal b of
		Nothing -> go' b Nothing
		Just (adj, b') -> go' b' (Just adj)
	go' b madj = do
		vu <- annexViewUnsetDirectory <$> Annex.getGitConfig
		let v = fst $ refineView (View b []) $
			map (parseViewParam vu) (reverse ps)
		return (v, madj)

checkoutViewBranch :: View -> Maybe Adjustment -> (View -> Maybe Adjustment -> Annex Git.Branch) -> CommandCleanup
checkoutViewBranch view madj mkbranch = do
	here <- liftIO getCurrentDirectory

	branch <- mkbranch view madj
	
	showOutput
	ok <- inRepo $ Git.Command.runBool
		[ Param "checkout"
		, Param (Git.fromRef $ Git.Ref.base branch)
		]
	when ok $ do
		setView view
		{- A git repo can easily have empty directories in it,
		 - and this pollutes the view, so remove them.
		 - (However, empty directories used by submodules are not
		 - removed.) -}
		top <- liftIO . absPath =<< fromRepo Git.repoPath
		(l, cleanup) <- inRepo $
			LsFiles.notInRepoIncludingEmptyDirectories [] False [top]
		forM_ l (removeemptydir top)
		liftIO $ void cleanup
		unlessM (liftIO $ doesDirectoryExist here) $ do
			showLongNote (cwdmissing (fromRawFilePath top))
	return ok
  where
	removeemptydir top d = do
		p <- inRepo $ toTopFilePath d
		liftIO $ tryIO $ removeDirectory $
			fromRawFilePath $ (top P.</> getTopFilePath p)
	cwdmissing top = unlines
		[ "This view does not include the subdirectory you are currently in."
		, "Perhaps you should:  cd " ++ top
		]

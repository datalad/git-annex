{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.View where

import Command
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
		view <- mkView ps
		go view  =<< currentView
	, giveup "Not safe to enter view."
	)
  where
	ai = ActionItemOther Nothing
	si = SeekInput ps
	go view Nothing = starting "view" ai si $
		perform view
	go view (Just v)
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

perform :: View -> CommandPerform
perform view = do
	showAction "searching"
	next $ checkoutViewBranch view applyView

paramView :: String
paramView = paramRepeating "FIELD=VALUE"

mkView :: [String] -> Annex View
mkView ps = go =<< inRepo Git.Branch.current
  where
	go Nothing = giveup "not on any branch!"
	go (Just b) = return $ fst $ refineView (View b []) $
		map parseViewParam $ reverse ps

checkoutViewBranch :: View -> (View -> Annex Git.Branch) -> CommandCleanup
checkoutViewBranch view mkbranch = do
	here <- liftIO getCurrentDirectory

	branch <- mkbranch view
	
	showOutput
	ok <- inRepo $ Git.Command.runBool
		[ Param "checkout"
		, Param (Git.fromRef $ Git.Ref.base branch)
		]
	when ok $ do
		setView view
		{- A git repo can easily have empty directories in it,
		 - and this pollutes the view, so remove them.
		 - (However, emptry directories used by submodules are not
		 - removed.) -}
		top <- liftIO . absPath . fromRawFilePath =<< fromRepo Git.repoPath
		(l, cleanup) <- inRepo $
			LsFiles.notInRepoIncludingEmptyDirectories [] False
				[toRawFilePath top]
		forM_ l (removeemptydir top)
		liftIO $ void cleanup
		unlessM (liftIO $ doesDirectoryExist here) $ do
			showLongNote (cwdmissing top)
	return ok
  where
	removeemptydir top d = do
		p <- inRepo $ toTopFilePath d
		liftIO $ tryIO $ removeDirectory (top </> fromRawFilePath (getTopFilePath p))
	cwdmissing top = unlines
		[ "This view does not include the subdirectory you are currently in."
		, "Perhaps you should:  cd " ++ top
		]

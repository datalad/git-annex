{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.VPop where

import Command
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import Types.View
import Logs.View
import Command.View (checkoutViewBranch)

cmd :: Command
cmd = notBareRepo $
	command "vpop" SectionMetaData "switch back to previous view"
		paramNumber (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start ps = go =<< currentView
  where
	go Nothing = giveup "Not in a view."
	go (Just (v, madj)) = starting "vpop" ai si $ do
		(oldvs, vs) <- splitAt (num - 1) 
			. filter (sameparentbranch v)
			. filter (/= v)
			<$> recentViews
		let removeview = mapM_ removeView (v : oldvs)
		ok <- case vs of
			(oldv:_) -> do
				showOutput
				checkoutViewBranch oldv madj
					(\v' madj' -> return (branchView v' madj'))
			_ -> do
				showOutput
				inRepo $ Git.Command.runBool
					[ Param "checkout"
					, Param $ Git.fromRef $ Git.Ref.base $
						viewParentBranch v
					]
		if ok
			then
				next $ do
					removeview
					return True
			else next $ return False
	sameparentbranch a b = viewParentBranch a == viewParentBranch b

	num = fromMaybe 1 $ readish =<< headMaybe ps 
	
	ai = ActionItemOther (Just $ UnquotedString $ show num)
	
	si = SeekInput ps

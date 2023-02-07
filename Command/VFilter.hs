{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.VFilter where

import Command
import qualified Annex
import Annex.View
import Command.View (paramView, checkoutViewBranch)

cmd :: Command
cmd = notBareRepo $
	command "vfilter" SectionMetaData "filter current view"
		paramView (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start params = starting "vfilter" (ActionItemOther Nothing) (SeekInput params) $
	withCurrentView $ \view -> do
		vu <- annexViewUnsetDirectory <$> Annex.getGitConfig
		let view' = filterView view $
			map (parseViewParam vu) (reverse params)
		next $ if visibleViewSize view' > visibleViewSize view
			then giveup "That would add an additional level of directory structure to the view, rather than filtering it. If you want to do that, use vadd instead of vfilter."
			else checkoutViewBranch view' narrowView

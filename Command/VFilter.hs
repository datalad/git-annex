{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.VFilter where

import Common.Annex
import Command
import Annex.View
import Command.View (paramView, parseViewParam, checkoutViewBranch)

def :: [Command]
def = [notBareRepo $ notDirect $
	command "vfilter" paramView seek SectionMetaData "filter current view"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start params = do
	showStart "vfilter" ""
	withCurrentView $ \view -> do
		let view' = filterView view $
			map parseViewParam $ reverse params
		next $ next $ if visibleViewSize view' > visibleViewSize view
			then error "That would add an additional level of directory structure to the view, rather than filtering it. If you want to do that, use vadd instead of vfilter."
			else checkoutViewBranch view' narrowView

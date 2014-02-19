{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.VAdd where

import Common.Annex
import Command
import Annex.View
import Logs.View
import Command.View (paramView, parseViewParam, checkoutViewBranch)

def :: [Command]
def = [notBareRepo $ notDirect $
	command "vadd" paramView seek SectionMetaData "refine current view"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start params = do
	showStart "vadd" ""
	go =<< currentView
  where
	go Nothing = error "Not in a view."
	go (Just view) = do
		let (view', change) = calc view Unchanged (reverse params)
		case change of
			Unchanged -> do
				showNote "unchanged"
				next $ next $ return True
			Narrowing -> next $ next $
				checkoutViewBranch view' narrowView
			Widening -> error "Widening view to match more files is not currently supported."

	calc v c [] = (v, c)
	calc v c (p:ps) =
		let (v', c') = uncurry (refineView v) (parseViewParam p)
		in calc v' (max c c') ps

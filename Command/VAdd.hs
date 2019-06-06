{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.VAdd where

import Command
import Annex.View
import Command.View (checkoutViewBranch)

cmd :: Command
cmd = notBareRepo $ notDirect $
	command "vadd" SectionMetaData 
		"add subdirs to current view"
		(paramRepeating "FIELD=GLOB")
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start params = starting "vadd" (ActionItemOther Nothing) $ 
	withCurrentView $ \view -> do
		let (view', change) = refineView view $
			map parseViewParam $ reverse params
		case change of
			Unchanged -> do
				showNote "unchanged"
				next $ return True
			Narrowing -> next $ do
				if visibleViewSize view' == visibleViewSize view
					then giveup "That would not add an additional level of directory structure to the view. To filter the view, use vfilter instead of vadd."
					else checkoutViewBranch view' narrowView
			Widening -> giveup "Widening view to match more files is not currently supported."

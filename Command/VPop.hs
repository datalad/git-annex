{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.VPop where

import Common.Annex
import Command
import qualified Git.Command
import qualified Git.Ref
import Types.View
import Logs.View
import Command.View (checkoutViewBranch)

def :: [Command]
def = [notBareRepo $ notDirect $
	command "vpop" paramNothing seek SectionUtility
	"switch back to previous view"]

seek :: CommandSeek
seek = withNothing start

start ::CommandStart
start = go =<< currentView
  where
	go Nothing = error "Not in a view."
	go (Just v) = do
		vs <- dropWhile (/= v) . filter (sameparentbranch v)
			<$> recentViews
		case vs of
			(_v:oldv:_) -> next $ next $ 
				checkoutViewBranch oldv (branchView oldv)
			_ -> next $ next $
				inRepo $ Git.Command.runBool
					[ Param "checkout"
					, Param $ show $ Git.Ref.base $
						viewParentBranch v
					]
	sameparentbranch a b = viewParentBranch a == viewParentBranch b

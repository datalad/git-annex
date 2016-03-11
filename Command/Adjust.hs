{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Adjust where

import Command
import Annex.AdjustedBranch

cmd :: Command
cmd = notBareRepo $ notDirect $
	command "adjust" SectionSetup "adjust branch"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = do
	enterAdjustedBranch HideMissingAdjustment
	next $ next $ return True
start _ = error "Unknown parameter"

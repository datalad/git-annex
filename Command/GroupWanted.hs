{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.GroupWanted where

import Common.Annex
import Command
import Logs.PreferredContent
import Command.Wanted (performGet, performSet)

cmd :: Command
cmd = command "groupwanted" SectionSetup 
	"get or set groupwanted expression"
	(paramPair paramGroup (paramOptional paramExpression))
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (g:[]) = next $ performGet groupPreferredContentMapRaw g
start (g:expr:[]) = do
	showStart "groupwanted" g
	next $ performSet groupPreferredContentSet expr g
start _ = error "Specify a group."

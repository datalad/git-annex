{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.GroupWanted where

import Command
import Logs.PreferredContent
import Command.Wanted (performGet, performSet)
import Types.Group

cmd :: Command
cmd = noMessages $ command "groupwanted" SectionSetup 
	"get or set groupwanted expression"
	(paramPair paramGroup (paramOptional paramExpression))
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (g:[]) = next $ performGet groupPreferredContentMapRaw (toGroup g)
start (g:expr:[]) = do
	allowMessages
	showStart' "groupwanted" (Just g)
	next $ performSet groupPreferredContentSet expr (toGroup g)
start _ = giveup "Specify a group."

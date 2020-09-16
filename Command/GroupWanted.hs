{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
start (g:[]) = startingCustomOutput (ActionItemOther Nothing) $
	performGet groupPreferredContentMapRaw (toGroup g)
start ps@(g:expr:[]) = startingUsualMessages "groupwanted" ai si $
	performSet groupPreferredContentSet expr (toGroup g)
  where
	ai = ActionItemOther (Just g)
	si = SeekInput ps
start _ = giveup "Specify a group."

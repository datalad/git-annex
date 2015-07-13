{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FindRef where

import Common.Annex
import Command
import qualified Command.Find as Find

cmd :: Command
cmd = withGlobalOptions nonWorkTreeMatchingOptions $ Find.mkCommand $ 
	command "findref" SectionPlumbing
		"lists files in a git ref"
		paramRef (seek <$$> Find.optParser)

seek :: Find.FindOptions -> CommandSeek
seek o = Find.start o `withFilesInRefs` Find.findThese o

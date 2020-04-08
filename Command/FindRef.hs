{- git-annex command
 -
 - Copyright 2014-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.FindRef where

import Command
import qualified Command.Find as Find
import qualified Git

cmd :: Command
cmd = withGlobalOptions [annexedMatchingOptions] $ Find.mkCommand $ 
	command "findref" SectionPlumbing
		"lists files in a git ref (deprecated)"
		paramRef (seek <$$> Find.optParser)

seek :: Find.FindOptions -> CommandSeek
seek o = Find.seek o'
  where
	o' = o 
		{ Find.keyOptions = Just $ WantBranchKeys $
			map (Git.Ref . encodeBS') (Find.findThese o)
		, Find.findThese = []
		}

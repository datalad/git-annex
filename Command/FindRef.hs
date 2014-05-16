{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FindRef where

import Command
import qualified Command.Find as Find

def :: [Command]
def = [Find.mkCommand $ command "findref" paramRef seek SectionPlumbing
	"lists files in a git ref"]

seek :: CommandSeek
seek refs = do
	format <- Find.getFormat
	Find.start format `withFilesInRefs` refs

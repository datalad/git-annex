{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ExamineKey where

import Common.Annex
import Command
import qualified Utility.Format
import Command.Find (formatOption, getFormat, showFormatted, keyVars)
import Types.Key
import GitAnnex.Options

def :: [Command]
def = [noCommit $ noMessages $ withOptions [formatOption, jsonOption] $
	command "examinekey" (paramRepeating paramKey) seek
	SectionPlumbing "prints information from a key"]

seek :: CommandSeek
seek ps = do
	format <- getFormat
	withKeys (start format) ps

start :: Maybe Utility.Format.Format -> Key -> CommandStart
start format key = do
	showFormatted format (key2file key) (keyVars key)
	stop

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
import Command.Find (formatOption, withFormat, showFormatted, keyVars)
import Types.Key

def :: [Command]
def = [noCommit $ noMessages $ withOptions [formatOption] $
	command "examinekey" (paramRepeating paramKey) seek
	SectionPlumbing "prints information from a key"]

seek :: [CommandSeek]
seek = [withFormat $ \f -> withKeys $ start f]

start :: Maybe Utility.Format.Format -> Key -> CommandStart
start format key = do
	showFormatted format (key2file key) (keyVars key)
	stop

{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ExamineKey where

import Common.Annex
import Command
import CmdLine.Batch
import qualified Utility.Format
import Command.Find (FindOptions(..), showFormatted, keyVars)
import Types.Key

cmd :: Command
cmd = noCommit $ noMessages $ withOptions [formatOption, jsonOption, batchOption] $
	command "examinekey" SectionPlumbing 
		"prints information from a key"
		(paramRepeating paramKey) (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = do
	format <- getFormat
	batchable withKeys (start format) ps

start :: Maybe Utility.Format.Format -> Batchable Key
start format _ key = do
	showFormatted format (key2file key) (keyVars key)
	stop

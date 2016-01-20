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
import Command.Find (parseFormatOption, showFormatted, keyVars)
import Types.Key

cmd :: Command
cmd = noCommit $ withGlobalOptions [jsonOption] $
	command "examinekey" SectionPlumbing 
		"prints information from a key"
		(paramRepeating paramKey)
		(batchable run (optional parseFormatOption))

run :: Maybe Utility.Format.Format -> String -> Annex Bool
run format p = do
	let k = fromMaybe (error "bad key") $ file2key p
	showFormatted format (key2file k) (keyVars k)
	return True

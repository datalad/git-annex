{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ExamineKey where

import Command
import qualified Utility.Format
import Command.Find (parseFormatOption, showFormatted, keyVars)

cmd :: Command
cmd = noCommit $ noMessages $ dontCheck repoExists $ 
	withGlobalOptions [jsonOption] $
		command "examinekey" SectionPlumbing 
			"prints information from a key"
			(paramRepeating paramKey)
			(batchable run (optional parseFormatOption))

run :: Maybe Utility.Format.Format -> String -> Annex Bool
run format p = do
	let k = fromMaybe (giveup "bad key") $ file2key p
	showFormatted format (key2file k) (keyVars k)
	return True

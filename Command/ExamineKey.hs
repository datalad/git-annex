{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.ExamineKey where

import Command
import qualified Utility.Format
import Command.Find (parseFormatOption, showFormatted, keyVars)

cmd :: Command
cmd = noCommit $ noMessages $ dontCheck repoExists $ 
	withGlobalOptions [jsonOptions] $
		command "examinekey" SectionPlumbing 
			"prints information from a key"
			(paramRepeating paramKey)
			(batchable run (optional parseFormatOption))

run :: Maybe Utility.Format.Format -> SeekInput -> String -> Annex Bool
run format _ p = do
	let k = fromMaybe (giveup "bad key") $ deserializeKey p
	showFormatted format (serializeKey' k) (keyVars k)
	return True

{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.LookupKey where

import Common.Annex
import Command
import CmdLine.Batch
import Annex.CatFile
import Types.Key

cmd :: Command
cmd = notBareRepo $ noCommit $
	command "lookupkey" SectionPlumbing 
		"looks up key used for file"
		(paramRepeating paramFile)
		(batchable run (pure ()))

run :: () -> String -> Annex Bool
run _ file = do
	mk <- catKeyFile file
	case mk of
		Just k  -> do
			liftIO $ putStrLn $ key2file k
			return True
		Nothing -> return False

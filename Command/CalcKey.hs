{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.CalcKey where

import Command
import Backend (genKey)
import Types.KeySource

cmd :: Command
cmd = noCommit $ noMessages $ dontCheck repoExists $
	command "calckey" SectionPlumbing 
		"calulate key for a file"
		(paramRepeating paramFile)
		(batchable run (pure ()))

run :: () -> String -> Annex Bool
run _ file = genKey (KeySource file file Nothing) Nothing >>= \case
	Just (k, _) -> do
		liftIO $ putStrLn $ serializeKey k
		return True
	Nothing -> return False

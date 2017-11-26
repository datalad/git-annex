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
		"calculates the key that would be used to refer to a file"
		(paramRepeating paramFile)
		(batchable run (pure ()))

run :: () -> String -> Annex Bool
run _ file = do
	mkb <- genKey (KeySource file file Nothing) Nothing
	case mkb of
		Just (k, _) -> do
			liftIO $ putStrLn $ key2file k
			return True
		Nothing -> return False

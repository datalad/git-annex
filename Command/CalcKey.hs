{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.CalcKey where

import Command
import Backend (genKey)
import Types.KeySource
import Utility.Metered

cmd :: Command
cmd = noCommit $ noMessages $ dontCheck repoExists $
	withAnnexOptions [backendOption] $
		command "calckey" SectionPlumbing 
			"calculate key for a file"
			(paramRepeating paramFile)
			(batchable run (pure ()))

run :: () -> SeekInput -> String -> Annex Bool
run _ _ file = tryNonAsync (genKey ks nullMeterUpdate Nothing) >>= \case
	Right (k, _) -> do
		liftIO $ putStrLn $ serializeKey k
		return True
	Left _err -> return False
  where
	ks = KeySource file' file' Nothing
	file' = toRawFilePath file

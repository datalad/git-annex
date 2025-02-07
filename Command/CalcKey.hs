{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.CalcKey where

import Command
import Backend (genKey, defaultBackend)
import Types.KeySource
import Utility.Metered
import Utility.Terminal
import Utility.SafeOutput

cmd :: Command
cmd = noCommit $ noMessages $ dontCheck repoExists $
	withAnnexOptions [backendOption] $
		command "calckey" SectionPlumbing 
			"calculate key for a file"
			(paramRepeating paramFile)
			(batchable run (pure ()))

run :: () -> SeekInput -> String -> Annex Bool
run _ _ file = tryNonAsync (genKey ks nullMeterUpdate =<< defaultBackend) >>= \case
	Right (k, _) -> do
		IsTerminal isterminal <- liftIO $ checkIsTerminal stdout
		let sk = serializeKey k
		liftIO $ putStrLn $ if isterminal then safeOutput sk else sk
		return True
	Left _err -> return False
  where
	ks = KeySource file' file' Nothing
	file' = toOsPath file

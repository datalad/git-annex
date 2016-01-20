{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ContentLocation where

import Common.Annex
import Command
import CmdLine.Batch
import Annex.Content
import Types.Key

cmd :: Command
cmd = noCommit $ 
	command "contentlocation" SectionPlumbing 
		"looks up content for a key"
		(paramRepeating paramKey)
		(batchable run (pure ()))

run :: () -> String -> Annex Bool
run _ p = do
	let k = fromMaybe (error "bad key") $ file2key p
	maybe (return False) (\f -> liftIO (putStrLn f) >> return True)
		=<< inAnnex' (pure True) Nothing check k
  where
	check f = ifM (liftIO (doesFileExist f))
		( return (Just f)
		, return Nothing
		)

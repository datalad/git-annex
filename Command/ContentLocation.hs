{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.ContentLocation where

import Command
import Annex.Content
import qualified Utility.RawFilePath as R

import qualified Data.ByteString.Char8 as B8

cmd :: Command
cmd = noCommit $ noMessages $
	command "contentlocation" SectionPlumbing 
		"looks up content for a key"
		(paramRepeating paramKey)
		(batchable run (pure ()))

run :: () -> SeekInput -> String -> Annex Bool
run _ _ p = do
	let k = fromMaybe (giveup "bad key") $ deserializeKey p
	maybe (return False) (\f -> liftIO (B8.putStrLn f) >> return True)
		=<< inAnnex' (pure True) Nothing check k
  where
	check f = ifM (liftIO (R.doesPathExist f))
		( return (Just f)
		, return Nothing
		)

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

cmd :: Command
cmd = withOptions [batchOption] $ noCommit $ noMessages $
	command "contentlocation" SectionPlumbing 
		"looks up content for a key"
		(paramRepeating paramKey) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = batchable withKeys start

start :: Batchable Key
start batchmode k = do
	maybe (batchBadInput batchmode) (liftIO . putStrLn)
		=<< inAnnex' (pure True) Nothing check k
	stop
  where
	check f = ifM (liftIO (doesFileExist f))
		( return (Just f)
		, return Nothing
		)

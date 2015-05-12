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

cmd :: [Command]
cmd = [withOptions [batchOption] $ notBareRepo $ noCommit $ noMessages $
	command "lookupkey" (paramRepeating paramFile) seek
		SectionPlumbing "looks up key used for file"]

seek :: CommandSeek
seek = batchable withStrings start

start :: Batchable String
start batchmode file = do
	maybe (batchBadInput batchmode) (liftIO . putStrLn . key2file)
		=<< catKeyFile file
	stop

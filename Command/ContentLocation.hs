{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ContentLocation where

import Common.Annex
import Command
import Annex.Content
import Types.Key

cmd :: [Command]
cmd = [noCommit $ noMessages $
	command "contentlocation" (paramRepeating paramKey) seek
		SectionPlumbing "looks up content for a key"]

seek :: CommandSeek
seek = withKeys start

start :: Key -> CommandStart
start k = do
	liftIO . maybe exitFailure putStrLn
		=<< inAnnex' (pure True) Nothing check k
	stop
  where
	check f = ifM (liftIO (doesFileExist f))
		( return (Just f)
		, return Nothing
		)

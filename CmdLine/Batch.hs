{- git-annex batch commands
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Batch where

import Common.Annex
import Command

batchOption :: Option
batchOption = flagOption [] "batch" "enable batch mode"

data BatchMode = Batch | NoBatch
type Batchable t = BatchMode -> t -> CommandStart

-- A Batchable command can run in batch mode, or not.
-- In batch mode, one line at a time is read, parsed, and a reply output to
-- stdout. In non batch mode, the command's parameters are parsed and
-- a reply output for each.
batchable :: ((t -> CommandStart) -> CommandSeek) -> Batchable t -> CommandSeek
batchable seeker starter params = ifM (getOptionFlag batchOption)
	( batchloop
	, seeker (starter NoBatch) params
	)
  where
	batchloop = do
		mp <- liftIO $ catchMaybeIO getLine
		case mp of
			Nothing -> return ()
			Just p -> do
				seeker (starter Batch) [p]
				batchloop

-- bad input is indicated by an empty line in batch mode. In non batch
-- mode, exit on bad input.
batchBadInput :: BatchMode -> Annex ()
batchBadInput NoBatch = liftIO exitFailure
batchBadInput Batch = liftIO $ putStrLn ""

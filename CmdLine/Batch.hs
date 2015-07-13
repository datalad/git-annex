{- git-annex batch commands
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Batch where

import Common.Annex
import Command

data BatchMode = Batch | NoBatch

batchOption :: Parser BatchMode
batchOption = flag NoBatch Batch
	( long "batch"
	<> help "enable batch mode"
	)

type Batchable t = BatchMode -> t -> CommandStart

-- A Batchable command can run in batch mode, or not.
-- In batch mode, one line at a time is read, parsed, and a reply output to
-- stdout. In non batch mode, the command's parameters are parsed and
-- a reply output for each.
batchable :: (opts -> String -> Annex Bool) -> Parser opts -> CmdParamsDesc -> CommandParser
batchable handler parser paramdesc = batchseeker <$> batchparser
  where
	batchparser = (,,)
		<$> parser
		<*> batchOption
		<*> cmdParams paramdesc
	
	batchseeker (opts, NoBatch, params) = mapM_ (go NoBatch opts) params
	batchseeker (opts, Batch, _) = batchloop opts

	batchloop opts = do
		mp <- liftIO $ catchMaybeIO getLine
		case mp of
			Nothing -> return ()
			Just p -> do
				go Batch opts p
				batchloop opts

	go batchmode opts p =
		unlessM (handler opts p) $
			batchBadInput batchmode

-- bad input is indicated by an empty line in batch mode. In non batch
-- mode, exit on bad input.
batchBadInput :: BatchMode -> Annex ()
batchBadInput NoBatch = liftIO exitFailure
batchBadInput Batch = liftIO $ putStrLn ""

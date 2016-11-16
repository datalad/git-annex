{- git-annex batch commands
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.Batch where

import Annex.Common
import Types.Command
import CmdLine.Action
import CmdLine.GitAnnex.Options
import Options.Applicative

data BatchMode = Batch | NoBatch

parseBatchOption :: Parser BatchMode
parseBatchOption = flag NoBatch Batch
	( long "batch"
	<> help "enable batch mode"
	)

-- A batchable command can run in batch mode, or not.
-- In batch mode, one line at a time is read, parsed, and a reply output to
-- stdout. In non batch mode, the command's parameters are parsed and
-- a reply output for each.
batchable :: (opts -> String -> Annex Bool) -> Parser opts -> CmdParamsDesc -> CommandParser
batchable handler parser paramdesc = batchseeker <$> batchparser
  where
	batchparser = (,,)
		<$> parser
		<*> parseBatchOption
		<*> cmdParams paramdesc
	
	batchseeker (opts, NoBatch, params) = mapM_ (go NoBatch opts) params
	batchseeker (opts, Batch, _) = batchInput Right (go Batch opts)

	go batchmode opts p =
		unlessM (handler opts p) $
			batchBadInput batchmode

-- bad input is indicated by an empty line in batch mode. In non batch
-- mode, exit on bad input.
batchBadInput :: BatchMode -> Annex ()
batchBadInput NoBatch = liftIO exitFailure
batchBadInput Batch = liftIO $ putStrLn ""

-- Reads lines of batch mode input and passes to the action to handle.
batchInput :: (String -> Either String a) -> (a -> Annex ()) -> Annex ()
batchInput parser a = do
	mp <- liftIO $ catchMaybeIO getLine
	case mp of
		Nothing -> return ()
		Just v -> do
			either parseerr a (parser v)
			batchInput parser a
  where
	parseerr s = giveup $ "Batch input parse failure: " ++ s

-- Runs a CommandStart in batch mode.
--
-- The batch mode user expects to read a line of output, and it's up to the
-- CommandStart to generate that output as it succeeds or fails to do its
-- job. However, if it stops without doing anything, it won't generate
-- any output, so in that case, batchBadInput is used to provide the caller
-- with an empty line.
batchCommandAction :: CommandStart -> Annex ()
batchCommandAction a = maybe (batchBadInput Batch) (const noop)
	=<< callCommandAction' a

-- Reads lines of batch input and passes the filepaths to a CommandStart
-- to handle them.
batchFiles :: (FilePath -> CommandStart) -> Annex ()
batchFiles a = batchInput Right $ batchCommandAction . a

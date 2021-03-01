{- git-annex batch commands
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.Batch where

import Annex.Common
import qualified Annex
import Types.Command
import CmdLine.Action
import CmdLine.GitAnnex.Options
import CmdLine.Seek
import Options.Applicative
import Limit
import Types.FileMatcher
import Annex.BranchState
import Annex.WorkTree
import Annex.Content
import Annex.Concurrent
import Types.Concurrency

data BatchMode = Batch BatchFormat | NoBatch

data BatchFormat = BatchLine | BatchNull

parseBatchOption :: Parser BatchMode
parseBatchOption = go 
	<$> switch
		( long "batch"
		<> help "enable batch mode"
		)
	<*> switch
		( short 'z'
		<> help "null delimited batch input"
		)
  where
	go True False = Batch BatchLine
 	go True True = Batch BatchNull
	go False _ = NoBatch

-- A batchable command can run in batch mode, or not.
-- In batch mode, one line at a time is read, parsed, and a reply output to
-- stdout. In non batch mode, the command's parameters are parsed and
-- a reply output for each.
--
-- Note that the actions are not run concurrently.
batchable :: (opts -> SeekInput -> String -> Annex Bool) -> Parser opts -> CmdParamsDesc -> CommandParser
batchable handler parser paramdesc = batchseeker <$> batchparser
  where
	batchparser = (,,)
		<$> parser
		<*> parseBatchOption
		<*> cmdParams paramdesc
	
	batchseeker (opts, NoBatch, params) =
		mapM_ (\p -> go NoBatch opts (SeekInput [p], p)) params
	batchseeker (opts, batchmode@(Batch fmt), _) = 
		batchInput fmt (pure . Right) (go batchmode opts)

	go batchmode opts (si, p) =
		unlessM (handler opts si p) $
			batchBadInput batchmode

-- bad input is indicated by an empty line in batch mode. In non batch
-- mode, exit on bad input.
batchBadInput :: BatchMode -> Annex ()
batchBadInput NoBatch = liftIO exitFailure
batchBadInput (Batch _) = liftIO $ putStrLn ""

-- Reads lines of batch mode input, runs a parser, and passes the result
-- to the action.
--
-- Note that if the batch input includes a worktree filename, it should
-- be converted to relative. Normally, filename parameters are passed
-- through git ls-files, which makes them relative, but batch mode does
-- not use that, and absolute worktree files are likely to cause breakage.
batchInput :: BatchFormat -> (String -> Annex (Either String v)) -> ((SeekInput, v) -> Annex ()) -> Annex ()
batchInput fmt parser a = go =<< batchLines fmt
  where
	go [] = return ()
	go (l:rest) = do
		either parseerr (\v -> a (SeekInput [l], v)) =<< parser l
		go rest
	parseerr s = giveup $ "Batch input parse failure: " ++ s

batchLines :: BatchFormat -> Annex [String]
batchLines fmt = do
	checkBatchConcurrency
	enableInteractiveBranchAccess
	liftIO $ splitter <$> getContents
  where
	splitter = case fmt of
		BatchLine -> lines
		BatchNull -> splitc '\0'

-- When concurrency is enabled at the command line, it is used in batch
-- mode. But, if it's only set in git config, don't use it, because the
-- program using batch mode may not expect interleaved output.
checkBatchConcurrency :: Annex ()
checkBatchConcurrency = Annex.getState Annex.concurrency >>= \case
	ConcurrencyCmdLine _ -> noop
	ConcurrencyGitConfig _ -> 
		setConcurrency (ConcurrencyGitConfig (Concurrent 1))

batchCommandAction :: CommandStart -> Annex ()
batchCommandAction = commandAction . batchCommandStart

-- The batch mode user expects to read a line of output, and it's up to the
-- CommandStart to generate that output as it succeeds or fails to do its
-- job. However, if it stops without doing anything, it won't generate
-- any output. This modifies it so in that case, an empty line is printed.
batchCommandStart :: CommandStart -> CommandStart
batchCommandStart a = a >>= \case
	Just v -> return (Just v)
	Nothing -> do
		batchBadInput (Batch BatchLine)
		return Nothing

-- Reads lines of batch input and passes the filepaths to a CommandStart
-- to handle them.
--
-- Absolute filepaths are converted to relative, because in non-batch
-- mode, that is done when CmdLine.Seek uses git ls-files.
--
-- File matching options are checked, and non-matching files skipped.
batchFilesMatching :: BatchFormat -> ((SeekInput, RawFilePath) -> CommandStart) -> Annex ()
batchFilesMatching fmt a = do
	matcher <- getMatcher
	go $ \si f ->
		let f' = toRawFilePath f
		in ifM (matcher $ MatchingFile $ FileInfo f' f' Nothing)
			( a (si, f')
			, return Nothing
			)
  where
	go a' = batchInput fmt 
		(Right . fromRawFilePath <$$> liftIO . relPathCwdToFile . toRawFilePath)
		(batchCommandAction . uncurry a')

batchAnnexedFilesMatching :: BatchFormat -> AnnexedFileSeeker -> Annex ()
batchAnnexedFilesMatching fmt seeker = batchFilesMatching fmt $ \(si, bf) ->
	flip whenAnnexed bf $ \f k -> 
		case checkContentPresent seeker of
			Just v -> do
				present <- inAnnex k
				if present == v
					then startAction seeker si f k
					else return Nothing
			Nothing -> startAction seeker si f k

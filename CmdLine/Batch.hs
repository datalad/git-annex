{- git-annex batch commands
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
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

data BatchFormat = BatchFormat BatchSeparator BatchKeys

data BatchSeparator = BatchLine | BatchNull

newtype BatchKeys = BatchKeys Bool

parseBatchOption :: Bool -> Parser BatchMode
parseBatchOption supportbatchkeysoption = go 
	<$> switch
		( long "batch"
		<> help batchhelp
		)
	<*> batchkeysswitch
	<*> flag BatchLine BatchNull
		( short 'z'
		<> help "null delimited batch input"
		)
  where
	go True False batchseparator = 
		Batch (BatchFormat batchseparator (BatchKeys False))
	go _ True batchseparator = 
		Batch (BatchFormat batchseparator (BatchKeys True))
	go _ _ _ = NoBatch

	batchhelp = "enable batch mode" ++
		if supportbatchkeysoption
			then ", with files input"
			else ""
	batchkeyshelp = "enable batch mode, with keys input"

	batchkeysswitch
		| supportbatchkeysoption = switch
			( long "batch-keys"
			<> help batchkeyshelp
			)
		| otherwise = pure False

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
		<*> parseBatchOption False
		<*> cmdParams paramdesc
	
	batchseeker (opts, NoBatch, params) =
		mapM_ (\p -> go NoBatch opts (SeekInput [p], p)) params
	batchseeker (opts, batchmode@(Batch fmt), params) = 
		batchOnly Nothing params $
			batchInput fmt (pure . Right) (go batchmode opts)

	go batchmode opts (si, p) =
		unlessM (handler opts si p) $
			batchBadInput batchmode

-- bad input is indicated by an empty line in batch mode. In non batch
-- mode, exit on bad input.
batchBadInput :: BatchMode -> Annex ()
batchBadInput NoBatch = liftIO exitFailure
batchBadInput _ = liftIO $ putStrLn ""

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
batchLines (BatchFormat sep _) = do
	checkBatchConcurrency
	enableInteractiveBranchAccess
	liftIO $ splitter <$> getContents
  where
	splitter = case sep of
		BatchLine -> lines
		BatchNull -> elimemptyend . splitc '\0'

	-- When there is a trailing null on the input, eliminate the empty
	-- string that splitc generates. Other empty strings elsewhere in
	-- the list are preserved. This is the same effect as how `lines`
	-- handles a trailing newline.
	elimemptyend [] = []
	elimemptyend (x:[])
		| null x = []
		| otherwise = [x]
	elimemptyend (x:rest) = x : elimemptyend rest

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
		batchBadInput (Batch (BatchFormat BatchLine (BatchKeys False)))
		return Nothing

-- Reads lines of batch input and passes the filepaths to a CommandStart
-- to handle them.
--
-- File matching options are checked, and non-matching files skipped.
batchFiles :: BatchFormat -> ((SeekInput, OsPath) -> CommandStart) -> Annex ()
batchFiles fmt a = batchFilesKeys fmt $ \(si, v) -> case v of
	Right f -> a (si, f)
	Left _k -> return Nothing

batchFilesKeys :: BatchFormat -> ((SeekInput, Either Key OsPath) -> CommandStart) -> Annex ()
batchFilesKeys fmt a = do
	matcher <- getMatcher
	go $ \si v -> case v of
		Right f -> 
			ifM (matcher $ MatchingFile $ FileInfo f f Nothing)
				( a (si, Right f)
				, return Nothing
				)
		Left k -> a (si, Left k)
  where
	go a' = batchInput fmt parser (batchCommandAction . uncurry a')
	parser = case fmt of
		-- Absolute filepaths are converted to relative,
		-- because in non-batch mode, that is done when
		-- CmdLine.Seek uses git ls-files.
		BatchFormat _ (BatchKeys False) -> 
			Right . Right
				<$$> liftIO . relPathCwdToFile . toOsPath
		BatchFormat _ (BatchKeys True) -> \i ->
			pure $ case deserializeKey i of
				Just k -> Right (Left k)
				Nothing -> Left "not a valid key"

batchAnnexedFiles :: BatchFormat -> AnnexedFileSeeker -> Annex ()
batchAnnexedFiles fmt seeker = batchAnnexed fmt seeker (const (return Nothing))

-- Reads lines of batch input and passes filepaths to the AnnexedFileSeeker
-- to handle them. Or, with --batch-keys, passes keys to the keyaction.
--
-- Matching options are checked, and non-matching items skipped.
batchAnnexed :: BatchFormat -> AnnexedFileSeeker -> ((SeekInput, Key, ActionItem) -> CommandStart) -> Annex ()
batchAnnexed fmt seeker keyaction = do
	matcher <- getMatcher
	batchFilesKeys fmt $ \(si, v) ->
		case v of
			Right f -> lookupKeyStaged f >>= \case
				Nothing -> return Nothing
				Just k -> checkpresent k $
					startAction seeker Nothing si f k
			Left k -> ifM (matcher (MatchingInfo (mkinfo k)))
				( checkpresent k $
					keyaction (si, k, mkActionItem k)
				, return Nothing)
  where
	checkpresent k cont = case checkContentPresent seeker of
		Just v -> do
			present <- inAnnex k
			if present == v
				then cont
				else return Nothing
		Nothing -> cont
	
	mkinfo k = ProvidedInfo
		{ providedFilePath = Nothing
		, providedKey = Just k
		, providedFileSize = Nothing
		, providedMimeType = Nothing
		, providedMimeEncoding = Nothing
		, providedLinkType = Nothing
		}

batchOnly :: Maybe KeyOptions -> CmdParams -> Annex () -> Annex ()
batchOnly Nothing [] a = a
batchOnly _ _ _ = giveup "Cannot combine batch option with file or key options"


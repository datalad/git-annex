{- git-annex command
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Copy where

import Command
import qualified Command.Move
import qualified Remote
import Annex.Wanted
import Annex.NumCopies

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "copy" SectionCommon
		"copy content of files to/from another repository"
		paramPaths (seek <--< optParser)

data CopyOptions = CopyOptions
	{ copyFiles :: CmdParams
	, fromToOptions :: Maybe FromToHereOptions
	, keyOptions :: Maybe KeyOptions
	, autoMode :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser CopyOptions
optParser desc = CopyOptions
	<$> cmdParams desc
	<*> parseFromToHereOptions
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseAutoOption
	<*> parseBatchOption True

instance DeferredParseClass CopyOptions where
	finishParse v = CopyOptions
		<$> pure (copyFiles v)
		<*> maybe (pure Nothing) (Just <$$> finishParse)
			(fromToOptions v)
		<*> pure (keyOptions v)
		<*> pure (autoMode v)
		<*> pure (batchOption v)

seek :: CopyOptions -> CommandSeek
seek o = case fromToOptions o of
	Just fto -> seek' o fto
	Nothing -> giveup "Specify --from or --to"

seek' :: CopyOptions -> FromToHereOptions -> CommandSeek
seek' o fto = startConcurrency (Command.Move.stages fto) $ do
	case batchOption o of
		NoBatch -> withKeyOptions
			(keyOptions o) (autoMode o) seeker
			(commandAction . keyaction)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (copyFiles o)
		Batch fmt -> batchOnly (keyOptions o) (copyFiles o) $
			batchAnnexed fmt seeker keyaction
  where
	ww = WarnUnmatchLsFiles "copy"
	
	seeker = AnnexedFileSeeker
		{ startAction = const $ start o fto
		, checkContentPresent = case fto of
			FromOrToRemote (FromRemote _) -> Just False
			FromOrToRemote (ToRemote _) -> Just True
			ToHere -> Just False
			FromRemoteToRemote _ _ -> Nothing
			FromAnywhereToRemote _ -> Nothing
		, usesLocationLog = True
		}
	keyaction = Command.Move.startKey NoLiveUpdate fto Command.Move.RemoveNever

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: CopyOptions -> FromToHereOptions -> SeekInput -> RawFilePath -> Key -> CommandStart
start o fto si file key = do
	ru <- case fto of
		FromOrToRemote (ToRemote dest) -> getru dest
		FromOrToRemote (FromRemote _) -> pure Nothing
		ToHere -> pure Nothing
		FromRemoteToRemote _ dest -> getru dest
		FromAnywhereToRemote dest -> getru dest
	lu <- prepareLiveUpdate ru key AddingKey
	start' lu o fto si file key
  where
	getru dest = Just . Remote.uuid <$> getParsed dest

start' :: LiveUpdate -> CopyOptions -> FromToHereOptions -> SeekInput -> RawFilePath -> Key -> CommandStart
start' lu o fto si file key = stopUnless shouldCopy $ 
	Command.Move.start lu fto Command.Move.RemoveNever si file key
  where
	shouldCopy
		| autoMode o = want <||> numCopiesCheck file key (<)
		| otherwise = return True
	want = case fto of
		FromOrToRemote (ToRemote dest) -> checkwantsend dest
		FromOrToRemote (FromRemote _) -> checkwantget
		ToHere -> checkwantget
		FromRemoteToRemote _ dest -> checkwantsend dest
		FromAnywhereToRemote dest -> checkwantsend dest

	checkwantsend dest = 
		(Remote.uuid <$> getParsed dest) >>=
			wantGetBy lu False (Just key) (AssociatedFile (Just file))
	checkwantget = wantGet lu False (Just key) (AssociatedFile (Just file))

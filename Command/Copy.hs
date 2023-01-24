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
seek' o fto = startConcurrend stages $ do
	case batchOption o of
		NoBatch -> withKeyOptions
			(keyOptions o) (autoMode o) seeker
			(commandAction . keyaction)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (copyFiles o)
		Batch fmt -> batchOnly (keyOptions o) (copyFiles o) $
			batchAnnexed fmt seeker keyaction
  where
	ww = WarnUnmatchLsFiles
	
	seeker = AnnexedFileSeeker
		{ startAction = start o fto
		, checkContentPresent = case fto of
			FromOrToRemote (FromRemote _) -> Just False
			FromOrToRemote (ToRemote _) -> Just True
			ToHere -> Just False
			FromRemoteToRemote _ _ -> Just False
		, usesLocationLog = True
		}
	keyaction = Command.Move.startKey fto Command.Move.RemoveNever

	stages = case fto of
		FromOrToRemote (FromRemote _) -> commandStages
		FromOrToRemote (ToRemote _) -> commandStages
		ToHere -> commandStages
		FromRemoteToRemote _ _ -> transferStages

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: CopyOptions -> FromToHereOptions -> SeekInput -> RawFilePath -> Key -> CommandStart
start o fto si file key = stopUnless shouldCopy $ 
	Command.Move.start fto Command.Move.RemoveNever si file key
  where
	shouldCopy
		| autoMode o = want <||> numCopiesCheck file key (<)
		| otherwise = return True
	want = case fto of
		FromOrToRemote (ToRemote dest) ->
			(Remote.uuid <$> getParsed dest) >>= checkwantsend
		FromOrToRemote (FromRemote _) -> checkwantget
		ToHere -> checkwantget
		FromRemoteToRemote _ dest ->
			(Remote.uuid <$> getParsed dest) >>= checkwantsend
			
	checkwantsend = wantGetBy False (Just key) (AssociatedFile (Just file))
	checkwantget = wantGet False (Just key) (AssociatedFile (Just file))

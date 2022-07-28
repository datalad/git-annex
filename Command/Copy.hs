{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
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
	, fromToOptions :: FromToHereOptions
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
		<*> finishParse (fromToOptions v)
		<*> pure (keyOptions v)
		<*> pure (autoMode v)
		<*> pure (batchOption v)

seek :: CopyOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
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
		{ startAction = start o
		, checkContentPresent = case fromToOptions o of
			Right (FromRemote _) -> Just False
			Right (ToRemote _) -> Just True
			Left ToHere -> Just False
		, usesLocationLog = True
		}
	keyaction = Command.Move.startKey (fromToOptions o) Command.Move.RemoveNever

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: CopyOptions -> SeekInput -> RawFilePath -> Key -> CommandStart
start o si file key = stopUnless shouldCopy $ 
	Command.Move.start (fromToOptions o) Command.Move.RemoveNever si file key
  where
	shouldCopy
		| autoMode o = want <||> numCopiesCheck file key (<)
		| otherwise = return True
	want = case fromToOptions o of
		Right (ToRemote dest) ->
			(Remote.uuid <$> getParsed dest) >>= checkwantsend
		Right (FromRemote _) -> checkwantget
		Left ToHere -> checkwantget
			
	checkwantsend = wantGetBy False (Just key) (AssociatedFile (Just file))
	checkwantget = wantGet False (Just key) (AssociatedFile (Just file))

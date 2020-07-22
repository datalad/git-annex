{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Mirror where

import Command
import qualified Command.Move
import qualified Command.Drop
import qualified Command.Get
import qualified Remote
import Annex.Content
import Annex.NumCopies
import Types.Transfer

cmd :: Command
cmd = withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "mirror" SectionCommon 
		"mirror content of files to/from another repository"
		paramPaths (seek <--< optParser)

data MirrorOptions = MirrorOptions
	{ mirrorFiles :: CmdParams
	, fromToOptions :: FromToOptions
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser MirrorOptions
optParser desc = MirrorOptions
	<$> cmdParams desc
	<*> parseFromToOptions
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)

instance DeferredParseClass MirrorOptions where
	finishParse v = MirrorOptions
		<$> pure (mirrorFiles v)
		<*> finishParse (fromToOptions v)
		<*> pure (keyOptions v)

seek :: MirrorOptions -> CommandSeek
seek o = startConcurrency stages $ 
	withKeyOptions (keyOptions o) False
		(commandAction . startKey o (AssociatedFile Nothing))
		(withFilesInGitAnnex ww seeker)
		=<< workTreeItems ww (mirrorFiles o)
  where
	stages = case fromToOptions o of
		FromRemote _ -> downloadStages
		ToRemote _ -> commandStages
	ww = WarnUnmatchLsFiles
	seeker = AnnexedFileSeeker
		{ startAction = start o
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

start :: MirrorOptions -> RawFilePath -> Key -> CommandStart
start o file k = startKey o afile (k, ai)
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (k, afile)

startKey :: MirrorOptions -> AssociatedFile -> (Key, ActionItem) -> CommandStart
startKey o afile (key, ai) = case fromToOptions o of
	ToRemote r -> checkFailedTransferDirection ai Upload $ ifM (inAnnex key)
		( Command.Move.toStart Command.Move.RemoveNever afile key ai =<< getParsed r
		, do
			numcopies <- getnumcopies
			Command.Drop.startRemote afile ai numcopies key =<< getParsed r
		)
	FromRemote r -> checkFailedTransferDirection ai Download $ do
		haskey <- flip Remote.hasKey key =<< getParsed r
		case haskey of
			Left _ -> stop
			Right True -> ifM (inAnnex key)
				( stop
				, Command.Get.start' (return True) Nothing key afile ai
				)
			Right False -> ifM (inAnnex key)
				( do
					numcopies <- getnumcopies
					Command.Drop.startLocal afile ai numcopies key []
				, stop
				)
  where
	getnumcopies = case afile of
		AssociatedFile Nothing -> getNumCopies
		AssociatedFile (Just af) -> getFileNumCopies (fromRawFilePath af)

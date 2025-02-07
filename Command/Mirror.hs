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
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
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
	withKeyOptions (keyOptions o) False seeker
		(commandAction . startKey o (AssociatedFile Nothing))
		(withFilesInGitAnnex ww seeker)
		=<< workTreeItems ww (mirrorFiles o)
  where
	stages = case fromToOptions o of
		FromRemote _ -> transferStages
		ToRemote _ -> commandStages
	ww = WarnUnmatchLsFiles "mirror"
	seeker = AnnexedFileSeeker
		{ startAction = const $ start o
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}

start :: MirrorOptions -> SeekInput -> OsPath -> Key -> CommandStart
start o si file k = startKey o afile (si, k, ai)
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (k, afile)

startKey :: MirrorOptions -> AssociatedFile -> (SeekInput, Key, ActionItem) -> CommandStart
startKey o afile (si, key, ai) = case fromToOptions o of
	ToRemote r -> checkFailedTransferDirection ai Upload $ ifM (inAnnex key)
		( Command.Move.toStart NoLiveUpdate Command.Move.RemoveNever afile key ai si =<< getParsed r
		, do
			(numcopies, mincopies) <- getSafestNumMinCopies afile key
			Command.Drop.startRemote NoLiveUpdate pcc afile ai si numcopies mincopies key (Command.Drop.DroppingUnused False)
				=<< getParsed r
		)
	FromRemote r -> checkFailedTransferDirection ai Download $ do
		haskey <- flip Remote.hasKey key =<< getParsed r
		case haskey of
			Left _ -> stop
			Right True -> ifM (inAnnex key)
				( stop
				, Command.Get.start' NoLiveUpdate (return True) Nothing key afile ai si
				)
			Right False -> ifM (inAnnex key)
				( do
					(numcopies, mincopies) <- getSafestNumMinCopies afile key
					Command.Drop.startLocal NoLiveUpdate pcc afile ai si numcopies mincopies key [] (Command.Drop.DroppingUnused False)
				, stop
				)
  where
	pcc = Command.Drop.PreferredContentChecked False

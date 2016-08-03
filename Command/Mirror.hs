{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
cmd = withGlobalOptions ([jobsOption] ++ annexedMatchingOptions) $
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
seek o = allowConcurrentOutput $ 
	withKeyOptions (keyOptions o) False
		(startKey o Nothing)
		(withFilesInGit $ whenAnnexed $ start o)
		(mirrorFiles o)

start :: MirrorOptions -> FilePath -> Key -> CommandStart
start o file k = startKey o afile k (mkActionItem afile)
  where
	afile = Just file

startKey :: MirrorOptions -> Maybe FilePath -> Key -> ActionItem -> CommandStart
startKey o afile key ai = case fromToOptions o of
	ToRemote r -> checkFailedTransferDirection ai Upload $ ifM (inAnnex key)
		( Command.Move.toStart False afile key ai =<< getParsed r
		, do
			numcopies <- getnumcopies
			Command.Drop.startRemote afile ai numcopies key =<< getParsed r
		)
	FromRemote r -> checkFailedTransferDirection ai Download $ do
		haskey <- flip Remote.hasKey key =<< getParsed r
		case haskey of
			Left _ -> stop
			Right True -> Command.Get.start' (return True) Nothing key afile ai
			Right False -> ifM (inAnnex key)
				( do
					numcopies <- getnumcopies
					Command.Drop.startLocal afile ai numcopies key []
				, stop
				)
  where
	getnumcopies = maybe getNumCopies getFileNumCopies afile

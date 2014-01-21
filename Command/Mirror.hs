{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Mirror where

import Common.Annex
import Command
import GitAnnex.Options
import qualified Command.Move
import qualified Command.Drop
import qualified Command.Get
import qualified Remote
import Annex.Content
import qualified Annex
import Config.NumCopies

def :: [Command]
def = [withOptions (fromToOptions ++ keyOptions) $
	command "mirror" paramPaths seek
		SectionCommon "mirror content of files to/from another repository"]

seek :: CommandSeek
seek ps = do
	to <- getOptionField toOption Remote.byNameWithUUID
	from <- getOptionField fromOption Remote.byNameWithUUID
	withKeyOptions
		(startKey to from Nothing)
		(withFilesInGit $ whenAnnexed $ start to from)
		ps

start :: Maybe Remote -> Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start to from file (key, _backend) = startKey to from (Just file) key

startKey :: Maybe Remote -> Maybe Remote -> Maybe FilePath -> Key -> CommandStart
startKey to from afile key = do
	noAuto
	case (from, to) of
		(Nothing, Nothing) -> error "specify either --from or --to"
		(Nothing, Just r) -> mirrorto r
		(Just r, Nothing) -> mirrorfrom r
		_ -> error "only one of --from or --to can be specified"
  where
	noAuto = whenM (Annex.getState Annex.auto) $
		error "--auto is not supported for mirror"
	mirrorto r = ifM (inAnnex key)
		( Command.Move.toStart r False afile key
		, do
			numcopies <- getnumcopies
			Command.Drop.startRemote afile numcopies key r
		)
	mirrorfrom r = do
		haskey <- Remote.hasKey r key
		case haskey of
			Left _ -> stop
			Right True -> Command.Get.start' (return True) Nothing key afile
			Right False -> ifM (inAnnex key)
				( do
					numcopies <- getnumcopies
					Command.Drop.startLocal afile numcopies key Nothing
				, stop
				)
	getnumcopies = maybe getNumCopies getFileNumCopies afile

{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Common.Annex
import Command
import qualified Command.Move
import qualified Remote
import Annex.Wanted

def :: [Command]
def = [withOptions Command.Move.options $ command "copy" paramPaths seek
	"copy content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withField Command.Move.toOption Remote.byName $ \to ->
		withField Command.Move.fromOption Remote.byName $ \from ->
			withFilesInGit $ whenAnnexed $ start to from]

{- A copy is just a move that does not delete the source file.
 - However, --auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: Maybe Remote -> Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start to from file (key, backend) = autoCopies file key (<) $
	stopUnless shouldCopy $ 
		Command.Move.start to from False file (key, backend)
	where
		shouldCopy = case to of
			Nothing -> checkAuto $ wantGet (Just file)
			Just r -> checkAuto $ wantSend (Just file) (Remote.uuid r)

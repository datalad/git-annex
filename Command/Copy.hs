{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Common.Annex
import Command
import GitAnnex.Options
import qualified Command.Move
import qualified Remote
import Annex.Wanted

def :: [Command]
def = [withOptions Command.Move.moveOptions $ command "copy" paramPaths seek
	SectionCommon "copy content of files to/from another repository"]

seek :: CommandSeek
seek ps = do
	to <- getOptionField toOption Remote.byNameWithUUID
	from <- getOptionField fromOption Remote.byNameWithUUID
	withKeyOptions
	 	(Command.Move.startKey to from False)
		(withFilesInGit $ whenAnnexed $ start to from)
		ps

{- A copy is just a move that does not delete the source file.
 - However, --auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: Maybe Remote -> Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start to from file (key, backend) = stopUnless shouldCopy $ 
	Command.Move.start to from False file (key, backend)
  where
	shouldCopy = checkAuto (check <||> numCopiesCheck file key (<))
	check = case to of
		Nothing -> wantGet False (Just file)
		Just r -> wantSend False (Just file) (Remote.uuid r)

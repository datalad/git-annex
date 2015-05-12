{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Common.Annex
import Command
import qualified Command.Move
import qualified Remote
import Annex.Wanted
import Annex.NumCopies

cmd :: [Command]
cmd = [withOptions copyOptions $ command "copy" paramPaths seek
	SectionCommon "copy content of files to/from another repository"]

copyOptions :: [Option]
copyOptions = Command.Move.moveOptions ++ [autoOption]

seek :: CommandSeek
seek ps = do
	to <- getOptionField toOption Remote.byNameWithUUID
	from <- getOptionField fromOption Remote.byNameWithUUID
	auto <- getOptionFlag autoOption
	withKeyOptions auto
		(Command.Move.startKey to from False)
		(withFilesInGit $ whenAnnexed $ start auto to from)
		ps

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: Bool -> Maybe Remote -> Maybe Remote -> FilePath -> Key -> CommandStart
start auto to from file key = stopUnless shouldCopy $ 
	Command.Move.start to from False file key
  where
	shouldCopy
		| auto = want <||> numCopiesCheck file key (<)
		| otherwise = return True
	want = case to of
		Nothing -> wantGet False (Just key) (Just file)
		Just r -> wantSend False (Just key) (Just file) (Remote.uuid r)

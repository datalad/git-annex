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

cmd :: Command
cmd = withGlobalOptions (jobsOption : annexedMatchingOptions) $
	command "copy" SectionCommon
		"copy content of files to/from another repository"
		paramPaths (seek <--< optParser)

data CopyOptions = CopyOptions
	{ moveOptions :: Command.Move.MoveOptions
	, autoMode :: Bool
	}

optParser :: CmdParamsDesc -> Parser CopyOptions
optParser desc = CopyOptions
	<$> Command.Move.optParser desc
	<*> parseAutoOption

instance DeferredParseClass CopyOptions where
	finishParse v = CopyOptions
		<$> finishParse (moveOptions v)
		<*> pure (autoMode v)

seek :: CopyOptions -> CommandSeek
seek o = withKeyOptions (Command.Move.keyOptions $ moveOptions o) (autoMode o)
	(Command.Move.startKey (moveOptions o) False)
	(withFilesInGit $ whenAnnexed $ start o)
	(Command.Move.moveFiles $ moveOptions o)

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: CopyOptions -> FilePath -> Key -> CommandStart
start o file key = stopUnless shouldCopy $ 
	Command.Move.start (moveOptions o) False file key
  where
	shouldCopy
		| autoMode o = want <||> numCopiesCheck file key (<)
		| otherwise = return True
	want = case Command.Move.fromToOptions (moveOptions o) of
		ToRemote _ -> 
			wantGet False (Just key) (Just file)
		FromRemote dest -> (Remote.uuid <$> getParsed dest) >>=
			wantSend False (Just key) (Just file)

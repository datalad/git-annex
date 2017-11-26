{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Command
import qualified Command.Move
import qualified Remote
import Annex.Wanted
import Annex.NumCopies

cmd :: Command
cmd = withGlobalOptions (jobsOption : jsonOption : jsonProgressOption : annexedMatchingOptions) $
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
seek o = allowConcurrentOutput $ do
	let go = whenAnnexed $ start o
	case Command.Move.batchOption (moveOptions o) of
		Batch -> batchInput Right (batchCommandAction . go)
		NoBatch -> withKeyOptions
			(Command.Move.keyOptions $ moveOptions o) (autoMode o)
			(Command.Move.startKey (moveOptions o) False)
			(withFilesInGit go)
			=<< workTreeItems (Command.Move.moveFiles $ moveOptions o)

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
		Right (ToRemote dest) ->
			(Remote.uuid <$> getParsed dest) >>= checkwantsend
		Right (FromRemote _) -> checkwantget
		Left Command.Move.ToHere -> checkwantget
			
	checkwantsend = wantSend False (Just key) (AssociatedFile (Just file))
	checkwantget = wantGet False (Just key) (AssociatedFile (Just file))

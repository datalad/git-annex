{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Common.Annex
import Command
import qualified Command.Add
import qualified Command.Fix
import qualified Git.DiffTree
import qualified Git.Ref
import Annex.CatFile
import Annex.Content.Direct
import Git.Sha
import Git.FilePath

def :: [Command]
def = [command "pre-commit" paramPaths seek SectionPlumbing
	"run by git pre-commit hook"]

seek :: [CommandSeek]
seek =
	-- fix symlinks to files being committed
	[ whenNotDirect $ withFilesToBeCommitted $ whenAnnexed Command.Fix.start
	-- inject unlocked files into the annex
	, whenNotDirect $ withFilesUnlockedToBeCommitted startIndirect
	-- update direct mode mappings for committed files
	, whenDirect $ withWords startDirect
	]

startIndirect :: FilePath -> CommandStart
startIndirect file = next $ do
	unlessM (doCommand $ Command.Add.start file) $
		error $ "failed to add " ++ file ++ "; canceling commit"
	next $ return True

startDirect :: [String] -> CommandStart
startDirect _ = next $ do
	(diffs, clean) <- inRepo $ Git.DiffTree.diffIndex Git.Ref.headRef
	makeabs <- flip fromTopFilePath <$> gitRepo
	forM_ diffs (go makeabs)
	next $ liftIO clean
  where
	go makeabs diff = do
		withkey (Git.DiffTree.srcsha diff) (Git.DiffTree.srcmode diff) removeAssociatedFile
		withkey (Git.DiffTree.dstsha diff) (Git.DiffTree.dstmode diff) addAssociatedFile
	  where
		withkey sha mode a = when (sha /= nullSha) $ do
			k <- catKey sha mode
			case k of
				Nothing -> noop
				Just key -> void $ a key $
					makeabs $ Git.DiffTree.file diff

{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ResolveMerge where

import Command
import qualified Git
import Git.Sha
import qualified Git.Branch
import Annex.AutoMerge

cmd :: Command
cmd = command "resolvemerge" SectionPlumbing
	"resolve merge conflicts"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	showStart "resolvemerge" ""
	us <- fromMaybe nobranch <$> inRepo Git.Branch.current
	d <- fromRepo Git.localGitDir
	let merge_head = d </> "MERGE_HEAD"
	them <- fromMaybe (error nomergehead) . extractSha
		<$> liftIO (readFile merge_head)
	ifM (resolveMerge (Just us) them False)
		( do
			void $ commitResolvedMerge Git.Branch.ManualCommit
			next $ next $ return True
		, giveup "Merge conflict could not be automatically resolved."
		)
  where
	nobranch = giveup "No branch is currently checked out."
	nomergehead = giveup "No SHA found in .git/merge_head"

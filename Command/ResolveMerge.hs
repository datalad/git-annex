{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.ResolveMerge where

import Command
import qualified Git
import Git.Sha
import qualified Git.Branch
import Annex.AutoMerge
import qualified Utility.FileIO as F

cmd :: Command
cmd = command "resolvemerge" SectionPlumbing
	"resolve merge conflicts"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = starting "resolvemerge" (ActionItemOther Nothing) (SeekInput []) $ do
	us <- fromMaybe nobranch <$> inRepo Git.Branch.current
	d <- fromRepo Git.localGitDir
	let merge_head = d </> literalOsPath "MERGE_HEAD"
	them <- fromMaybe (giveup nomergehead) . extractSha
		<$> liftIO (F.readFile' merge_head)
	ifM (resolveMerge (Just us) them False)
		( do
			void $ commitResolvedMerge Git.Branch.ManualCommit
			next $ return True
		, giveup "Merge conflict could not be automatically resolved."
		)
  where
	nobranch = giveup "No branch is currently checked out."
	nomergehead = giveup "No SHA found in .git/MERGE_HEAD"

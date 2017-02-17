{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.PostReceive where

import Command
import qualified Annex
import Config
import Annex.Version
import Annex.AdjustedBranch
import Git.Branch
import Git.Types
import Git.ConfigTypes
import qualified Command.Merge

cmd :: Command
cmd = command "post-receive" SectionPlumbing
	"run by git post-receive hook"
	paramNothing
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = whenM needUpdateInsteadEmulation $ do
	fixPostReceiveHookEnv
	updateInsteadEmulation

{- When run by the post-receive hook, the cwd is the .git directory, 
 - and GIT_DIR=. It's not clear why git does this.
 -
 - Fix up from that unusual situation, so that git commands
 - won't try to treat .git as the work tree. -}
fixPostReceiveHookEnv :: Annex ()
fixPostReceiveHookEnv = do
	g <- Annex.gitRepo
	case location g of
		Local { gitdir = ".", worktree = Just "." } ->
			Annex.adjustGitRepo $ \g' -> pure $ g'
				{ location = (location g')
					{ worktree = Just ".." }
				}
		_ -> noop

{- receive.denyCurrentBranch=updateInstead does not work in direct mode
 - repositories or when an adjusted branch is checked out, so must be
 - emulated. -}
needUpdateInsteadEmulation :: Annex Bool
needUpdateInsteadEmulation = updateinsteadset <&&> (isDirect <||> isadjusted)
  where
	updateinsteadset = (== UpdateInstead) . receiveDenyCurrentBranch
		<$> Annex.getGitConfig
	isadjusted = versionSupportsUnlockedPointers
		<&&> (maybe False (isJust . getAdjustment) <$> inRepo Git.Branch.current)

updateInsteadEmulation :: Annex ()
updateInsteadEmulation = commandAction Command.Merge.mergeSynced

{- git-annex direct mode guard fixup
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Direct.Fixup where

import Git.Types
import Git.Config
import qualified Git.Construct as Construct
import Utility.Path
import Utility.SafeCommand

{- Direct mode repos have core.bare=true, but are not really bare.
 - Fix up the Repo to be a non-bare repo, and arrange for git commands
 - run by git-annex to be passed parameters that override this setting. -}
fixupDirect :: Repo -> IO Repo
fixupDirect r@(Repo { location = l@(Local { gitdir = d, worktree = Nothing }) }) = do
	let r' = r
		{ location = l { worktree = Just (parentDir d) }
		, gitGlobalOpts = gitGlobalOpts r ++
			[ Param "-c"
			, Param $ coreBare ++ "=" ++ boolConfig False
			]
		}
	-- Recalc now that the worktree is correct.
	rs' <- Construct.fromRemotes r'
	return $ r' { remotes = rs' }
fixupDirect r = return r

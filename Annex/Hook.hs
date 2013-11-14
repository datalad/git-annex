{- git-annex git hooks
 -
 - Note that it's important that the scripts not change, otherwise
 - removing old hooks using an old version of the script would fail.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Hook where

import Common.Annex
import qualified Git.Hook as Git
import Utility.Shell
import Config

preCommitHook :: Git.Hook
preCommitHook = Git.Hook "pre-commit" (mkHookScript "git annex pre-commit .")

mkHookScript :: String -> String
mkHookScript s = unlines
	[ shebang_local
	, "# automatically configured by git-annex"
	, s
	]

hookWrite :: Git.Hook -> Annex ()
hookWrite h = 
	-- cannot have git hooks in a crippled filesystem (no execute bit)
	unlessM crippledFileSystem $
		unlessM (inRepo $ Git.hookWrite h) $
			hookWarning h "already exists, not configuring"

hookUnWrite :: Git.Hook -> Annex ()
hookUnWrite h = unlessM (inRepo $ Git.hookUnWrite h) $
	hookWarning h "contents modified; not deleting. Edit it to remove call to git annex."

hookWarning :: Git.Hook -> String -> Annex ()
hookWarning h msg = do
	r <- gitRepo
	warning $ Git.hookName h ++ " hook (" ++ Git.hookFile h r ++ ") " ++ msg

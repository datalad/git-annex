{- git-annex git hooks
 -
 - Note that it's important that the scripts installed by git-annex
 - not change, otherwise removing old hooks using an old version of
 - the script would fail.
 -
 - Copyright 2013-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Hook where

import Common.Annex
import qualified Git.Hook as Git
import Config
import qualified Annex
import Utility.Shell
import Utility.FileMode

import qualified Data.Map as M

preCommitHook :: Git.Hook
preCommitHook = Git.Hook "pre-commit" (mkHookScript "git annex pre-commit .")

preCommitAnnexHook :: Git.Hook
preCommitAnnexHook = Git.Hook "pre-commit-annex" ""

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

{- Runs a hook. To avoid checking if the hook exists every time,
 - the existing hooks are cached. -}
runAnnexHook :: Git.Hook -> Annex ()
runAnnexHook hook = do
	cmd <- fromRepo $ Git.hookFile hook
	m <- Annex.getState Annex.existinghooks
	case M.lookup hook m of
		Just True -> run cmd
		Just False -> noop
		Nothing -> do
			exists <- hookexists cmd
			Annex.changeState $ \s -> s
				{ Annex.existinghooks = M.insert hook exists m }
			when exists $
				run cmd
  where
	hookexists f = liftIO $ catchBoolIO $
		isExecutable . fileMode <$> getFileStatus f
	run cmd = unlessM (liftIO $ boolSystem cmd []) $
		warning $ cmd ++ " failed"

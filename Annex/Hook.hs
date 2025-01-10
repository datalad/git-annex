{- git-annex git hooks
 -
 - Note that it's important that the content of scripts installed by
 - git-annex not change, otherwise removing old hooks using an old
 - version of the script would fail.
 -
 - Copyright 2013-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Hook where

import Annex.Common
import qualified Git.Hook as Git
import qualified Annex
import Utility.Shell

import qualified Data.Map as M

preCommitHook :: Git.Hook
preCommitHook = Git.Hook "pre-commit" (mkHookScript "git annex pre-commit .") []

postReceiveHook :: Git.Hook
postReceiveHook = Git.Hook "post-receive"
	-- Only run git-annex post-receive when git-annex supports it,
	-- to avoid failing if the repository with this hook is used
	-- with an older version of git-annex.
	(mkHookScript "if git annex post-receive --help >/dev/null 2>&1; then git annex post-receive; fi")
	-- This is an old version of the hook script.
	[ mkHookScript "git annex post-receive"
	]

postCheckoutHook :: Git.Hook
postCheckoutHook = Git.Hook "post-checkout" smudgeHook []

postMergeHook :: Git.Hook
postMergeHook = Git.Hook "post-merge" smudgeHook []

-- Older versions of git-annex didn't support this command, but neither did
-- they support v7 repositories.
smudgeHook :: String
smudgeHook = mkHookScript "git annex smudge --update"

preCommitAnnexHook :: Git.Hook
preCommitAnnexHook = Git.Hook "pre-commit-annex" "" []

postUpdateAnnexHook :: Git.Hook
postUpdateAnnexHook = Git.Hook "post-update-annex" "" []

freezeContentAnnexHook :: Git.Hook
freezeContentAnnexHook = Git.Hook "freezecontent-annex" "" []

thawContentAnnexHook :: Git.Hook
thawContentAnnexHook = Git.Hook "thawcontent-annex" "" []

secureEraseAnnexHook :: Git.Hook
secureEraseAnnexHook = Git.Hook "secure-erase-annex" "" []

commitMessageAnnexHook :: Git.Hook
commitMessageAnnexHook = Git.Hook "commitmessage-annex" "" []

httpHeadersAnnexHook :: Git.Hook
httpHeadersAnnexHook = Git.Hook "http-headers-annex" "" []

mkHookScript :: String -> String
mkHookScript s = unlines
	[ shebang
	, "# automatically configured by git-annex"
	, s
	]

hookWrite :: Git.Hook -> Annex ()
hookWrite h = unlessM (inRepo $ Git.hookWrite h) $
	hookWarning h "already exists, not configuring"

hookUnWrite :: Git.Hook -> Annex ()
hookUnWrite h = unlessM (inRepo $ Git.hookUnWrite h) $
	hookWarning h "contents modified; not deleting. Edit it to remove call to git annex."

hookWarning :: Git.Hook -> String -> Annex ()
hookWarning h msg = do
	r <- gitRepo
	warning $ UnquotedString $
		Git.hookName h ++ " hook (" ++ Git.hookFile h r ++ ") " ++ msg

{- To avoid checking if the hook exists every time, the existing hooks
 - are cached. -}
doesAnnexHookExist :: Git.Hook -> Annex Bool
doesAnnexHookExist hook = do
	m <- Annex.getState Annex.existinghooks
	case M.lookup hook m of
		Just exists -> return exists
		Nothing -> do
			exists <- inRepo $ Git.hookExists hook
			Annex.changeState $ \s -> s
				{ Annex.existinghooks = M.insert hook exists m }
			return exists

runAnnexHook :: Git.Hook -> (GitConfig -> Maybe String) -> Annex ()
runAnnexHook hook commandcfg = ifM (doesAnnexHookExist hook)
	( runhook
	, runcommandcfg
	)
  where
	runhook = unlessM (inRepo $ Git.runHook boolSystem hook []) $ do
		h <- fromRepo $ Git.hookFile hook
		commandfailed h
	runcommandcfg = commandcfg <$> Annex.getGitConfig >>= \case
		Just command ->
			unlessM (liftIO $ boolSystem "sh" [Param "-c", Param command]) $
				commandfailed command
		Nothing -> noop
	commandfailed c = warning $ UnquotedString $ c ++ " failed"

runAnnexPathHook :: String -> Git.Hook -> (GitConfig -> Maybe String) -> RawFilePath -> Annex Bool
runAnnexPathHook pathtoken hook commandcfg p = ifM (doesAnnexHookExist hook)
	( runhook
	, runcommandcfg
	)
  where
	runhook = inRepo $ Git.runHook boolSystem hook [ File (fromRawFilePath p) ]
	runcommandcfg = commandcfg <$> Annex.getGitConfig >>= \case
		Just basecmd -> liftIO $
			boolSystem "sh" [Param "-c", Param $ gencmd basecmd]
		Nothing -> return True
	gencmd = massReplace [ (pathtoken, shellEscape (fromRawFilePath p)) ]

outputOfAnnexHook :: Git.Hook -> (GitConfig -> Maybe String) -> Annex (Maybe String)
outputOfAnnexHook hook commandcfg = ifM (doesAnnexHookExist hook)
	( runhook
	, runcommandcfg
	)
  where
	runhook = inRepo (Git.runHook runhook' hook [])
	runhook' c ps = Just <$> readProcess c (toCommand ps)
	runcommandcfg = commandcfg <$> Annex.getGitConfig >>= \case
		Just command -> liftIO $ 
			Just <$> readProcess "sh" ["-c", command]
		Nothing -> return Nothing

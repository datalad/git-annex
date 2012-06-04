{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unannex where

import Common.Annex
import Command
import qualified Annex
import Logs.Location
import Annex.Content
import qualified Git.Command
import qualified Git.LsFiles as LsFiles

def :: [Command]
def = [command "unannex" paramPaths seek "undo accidential add command"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = stopUnless (inAnnex key) $ do
	showStart "unannex" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform file key = next $ cleanup file key

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	liftIO $ removeFile file
	-- git rm deletes empty directory without --cached
	inRepo $ Git.Command.run "rm" [Params "--cached --quiet --", File file]
	
	-- If the file was already committed, it is now staged for removal.
	-- Commit that removal now, to avoid later confusing the
	-- pre-commit hook if this file is later added back to
	-- git as a normal, non-annexed file.
	whenM (not . null <$> inRepo (LsFiles.staged [file])) $ do
		showOutput
		inRepo $ Git.Command.run "commit" [
			Param "-q",
			Params "-m", Param "content removed from git annex",
			Param "--", File file]

	ifM (Annex.getState Annex.fast)
		( do
			-- fast mode: hard link to content in annex
			src <- inRepo $ gitAnnexLocation key
			liftIO $ createLink src file
			thawContent file
		, do
			fromAnnex key file
			logStatus key InfoMissing
		)

	return True

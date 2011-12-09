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
import Utility.FileMode
import Logs.Location
import Annex.Content
import qualified Git
import qualified Git.LsFiles as LsFiles

def :: [Command]
def = [command "unannex" paramPaths seek "undo accidential add command"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend Annex) -> CommandStart
start file (key, _) = stopUnless (inAnnex key) $ do
	showStart "unannex" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform file key = next $ cleanup file key

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	liftIO $ removeFile file
	-- git rm deletes empty directory without --cached
	inRepo $ Git.run "rm" [Params "--cached --quiet --", File file]
	
	-- If the file was already committed, it is now staged for removal.
	-- Commit that removal now, to avoid later confusing the
	-- pre-commit hook if this file is later added back to
	-- git as a normal, non-annexed file.
	whenM (not . null <$> inRepo (LsFiles.staged [file])) $ do
		inRepo $ Git.run "commit" [
			Param "-m", Param "content removed from git annex",
			Param "--", File file]

	fast <- Annex.getState Annex.fast
	if fast
		then do
			-- fast mode: hard link to content in annex
			src <- inRepo $ gitAnnexLocation key
			liftIO $ do
				createLink src file
				allowWrite file
		else do
			fromAnnex key file
			logStatus key InfoMissing

	return True

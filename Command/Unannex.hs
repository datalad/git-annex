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
import qualified Annex.Queue
import Utility.FileMode
import Logs.Location
import Annex.Content
import qualified Git
import qualified Git.LsFiles as LsFiles

def :: [Command]
def = [command "unannex" paramPaths seek "undo accidential add command"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

{- The unannex subcommand undoes an add. -}
start :: FilePath -> (Key, Backend Annex) -> CommandStart
start file (key, _) = stopUnless (inAnnex key) $ do
	force <- Annex.getState Annex.force
	unless force $ do
		top <- fromRepo Git.workTree
		staged <- inRepo $ LsFiles.staged [top]
		unless (null staged) $
			error "This command cannot be run when there are already files staged for commit."
		Annex.changeState $ \s -> s { Annex.force = True }

	showStart "unannex" file
	next $ perform file key

perform :: FilePath -> Key -> CommandPerform
perform file key = next $ cleanup file key

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	liftIO $ removeFile file
	inRepo $ Git.run "rm" [Params "--quiet --", File file]
	-- git rm deletes empty directories; put them back
	liftIO $ createDirectoryIfMissing True (parentDir file)

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
	
	-- Commit staged changes at end to avoid confusing the
	-- pre-commit hook if this file is later added back to
	-- git as a normal, non-annexed file.
	Annex.Queue.add "commit" [Param "-m", Param "content removed from git annex"] []
	
	return True

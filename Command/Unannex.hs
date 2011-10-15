{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unannex where

import Common.Annex
import Command
import qualified Command.Drop
import qualified Annex
import qualified Annex.Queue
import Utility.FileMode
import Logs.Location
import Annex.Content
import qualified Git
import qualified Git.LsFiles as LsFiles

command :: [Command]
command = [repoCommand "unannex" paramPaths seek "undo accidential add command"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- The unannex subcommand undoes an add. -}
start :: FilePath -> CommandStart
start file = isAnnexed file $ \(key, _) -> do
	ishere <- inAnnex key
	if ishere
		then do
			force <- Annex.getState Annex.force
			unless force $ do
				g <- gitRepo
				staged <- liftIO $ LsFiles.staged g [Git.workTree g]
				unless (null staged) $
					error "This command cannot be run when there are already files staged for commit."
				Annex.changeState $ \s -> s { Annex.force = True }

			showStart "unannex" file
			next $ perform file key
		else stop

perform :: FilePath -> Key -> CommandPerform
perform file key = do
	ok <- Command.Drop.dropKey key (Just 0) -- always remove
	if ok
		then next $ cleanup file key
		else stop

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	g <- gitRepo

	liftIO $ removeFile file
	liftIO $ Git.run g "rm" [Params "--quiet --", File file]
	-- git rm deletes empty directories; put them back
	liftIO $ createDirectoryIfMissing True (parentDir file)

	fast <- Annex.getState Annex.fast
	if fast
		then liftIO $ do
			-- fast mode: hard link to content in annex
			createLink (gitAnnexLocation g key) file
			allowWrite file
		else do
			fromAnnex key file
			logStatus key InfoMissing
	
	-- Commit staged changes at end to avoid confusing the
	-- pre-commit hook if this file is later added back to
	-- git as a normal, non-annexed file.
	Annex.Queue.add "commit" [Param "-m", Param "content removed from git annex"] []
	
	return True

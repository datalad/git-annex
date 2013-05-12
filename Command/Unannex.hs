{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Unannex where

import Common.Annex
import Command
import qualified Annex
import Logs.Location
import Annex.Content
import qualified Git.Command
import qualified Git.LsFiles as LsFiles

def :: [Command]
def = [notDirect $
	command "unannex" paramPaths seek SectionUtility
		"undo accidential add command"]

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
	inRepo $ Git.Command.run [Params "rm --cached --quiet --", File file]
	
	-- If the file was already committed, it is now staged for removal.
	-- Commit that removal now, to avoid later confusing the
	-- pre-commit hook if this file is later added back to
	-- git as a normal, non-annexed file.
	(s, clean) <- inRepo $ LsFiles.staged [file]
	when (not $ null s) $ do
		inRepo $ Git.Command.run
			[ Param "commit"
			, Param "-q"
			, Param "-m", Param "content removed from git annex"
			, Param "--", File file
			]
	void $ liftIO clean

	ifM (Annex.getState Annex.fast)
		( goFast
		, go
		)

	return True
  where
#ifdef __WINDOWS__
	goFast = go
#else
	goFast = do
		-- fast mode: hard link to content in annex
		src <- calcRepo $ gitAnnexLocation key
		-- creating a hard link could fall; fall back to non fast mode
		ifM (liftIO $ catchBoolIO $ createLink src file >> return True)
			( thawContent file
			, go
			)
#endif
	go = do
		fromAnnex key file
		logStatus key InfoMissing

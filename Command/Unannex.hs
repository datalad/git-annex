{- git-annex command
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Unannex where

import Common.Annex
import Command
import Config
import qualified Annex
import Logs.Location
import Annex.Content
import Annex.Content.Direct
import qualified Git.Command
import qualified Git.LsFiles as LsFiles

def :: [Command]
def = [command "unannex" paramPaths seek SectionUtility
		"undo accidential add command"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = stopUnless (inAnnex key) $ do
	showStart "unannex" file
	next $ ifM isDirect
		( performDirect file key
		, performIndirect file key)

performIndirect :: FilePath -> Key -> CommandPerform
performIndirect file key = do
	liftIO $ removeFile file
	
	-- git rm deletes empty directory without --cached
	inRepo $ Git.Command.run [Params "rm --cached --force --quiet --", File file]
	
	-- If the file was already committed, it is now staged for removal.
	-- Commit that removal now, to avoid later confusing the
	-- pre-commit hook, if this file is later added back to
	-- git as a normal non-annexed file, to thinking that the
	-- file has been unlocked and needs to be re-annexed.
	(s, reap) <- inRepo $ LsFiles.staged [file]
	when (not $ null s) $
		inRepo $ Git.Command.run
			[ Param "commit"
			, Param "-q"
			, Param "--no-verify"
			, Param "-m", Param "content removed from git annex"
			, Param "--", File file
			]
	void $ liftIO reap

	next $ cleanupIndirect file key

cleanupIndirect :: FilePath -> Key -> CommandCleanup
cleanupIndirect file key = do
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


performDirect :: FilePath -> Key -> CommandPerform
performDirect file key = do
	-- --force is needed when the file is not committed
	inRepo $ Git.Command.run [Params "rm --cached --force --quiet --", File file]
	next $ cleanupDirect file key

{- The direct mode file is not touched during unannex, so the content
 - is already where it needs to be, so this does not need to do anything
 - except remove it from the associated file map (which also updates
 - the location log if this was the last copy), and, if this was the last
 - associated file, remove the inode cache. -}
cleanupDirect :: FilePath -> Key -> CommandCleanup
cleanupDirect file key = do
	fs <- removeAssociatedFile key file
	when (null fs) $
		removeInodeCache key
	return True

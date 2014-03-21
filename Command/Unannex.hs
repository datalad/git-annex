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
import Annex.Content
import Annex.Content.Direct
import qualified Git.Command
import qualified Git.Ref
import qualified Git.DiffTree as DiffTree
import Utility.CopyFile
import Command.PreCommit (lockPreCommitHook)

def :: [Command]
def = [command "unannex" paramPaths seek SectionUtility
		"undo accidential add command"]

seek :: CommandSeek
seek = wrapUnannex . (withFilesInGit $ whenAnnexed start)

wrapUnannex :: Annex a -> Annex a
wrapUnannex a = ifM isDirect
	( a
	{- Run with the pre-commit hook disabled, to avoid confusing
	 - behavior if an unannexed file is added back to git as
	 - a normal, non-annexed file and then committed.
	 - Otherwise, the pre-commit hook would think that the file
	 - has been unlocked and needs to be re-annexed.
	 -
	 - At the end, make a commit removing the unannexed files.
	 -}
	, ifM cleanindex
		( lockPreCommitHook $ commit `after` a
		, error "Cannot proceed with uncommitted changes staged in the index. Recommend you: git commit"
		)
	)
  where
	commit = inRepo $ Git.Command.run
		[ Param "commit"
		, Param "-q"
		, Param "--allow-empty"
		, Param "--no-verify"
		, Param "-m", Param "content removed from git annex"
		]
	cleanindex = do
		(diff, cleanup) <- inRepo $ DiffTree.diffIndex Git.Ref.headRef
		if null diff
			then void (liftIO cleanup) >> return True
			else void (liftIO cleanup) >> return False

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = stopUnless (inAnnex key) $ do
	showStart "unannex" file
	next $ ifM isDirect
		( performDirect file key
		, performIndirect file key)

performIndirect :: FilePath -> Key -> CommandPerform
performIndirect file key = do
	liftIO $ removeFile file
	inRepo $ Git.Command.run [Params "rm --cached --force --quiet --", File file]
	next $ cleanupIndirect file key

cleanupIndirect :: FilePath -> Key -> CommandCleanup
cleanupIndirect file key = do
	src <- calcRepo $ gitAnnexLocation key
	ifM (Annex.getState Annex.fast)
		( hardlinkfrom src
		, copyfrom src
		)
  where
	copyfrom src = 
		thawContent file `after` liftIO (copyFileExternal src file)
	hardlinkfrom src =
#ifndef mingw32_HOST_OS
		-- creating a hard link could fall; fall back to copying
		ifM (liftIO $ catchBoolIO $ createLink src file >> return True)
			( return True
			, copyfrom src
			)
#else
		copyfrom src
#endif

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

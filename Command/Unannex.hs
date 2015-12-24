{- git-annex command
 -
 - Copyright 2010-2013 Joey Hess <id@joeyh.name>
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
import Annex.Version
import qualified Git.Command
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.DiffTree as DiffTree
import Utility.CopyFile
import Command.PreCommit (lockPreCommitHook)
import qualified Database.Keys

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "unannex" SectionUtility
		"undo accidential add command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = wrapUnannex . (withFilesInGit $ whenAnnexed start)

wrapUnannex :: Annex a -> Annex a
wrapUnannex a = ifM (versionSupportsUnlockedPointers <||> isDirect)
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
	commit = inRepo $ Git.Branch.commitCommand Git.Branch.ManualCommit
		[ Param "-q"
		, Param "--allow-empty"
		, Param "--no-verify"
		, Param "-m", Param "content removed from git annex"
		]
	cleanindex = ifM (inRepo Git.Ref.headExists)
		( do
			(diff, cleanup) <- inRepo $ DiffTree.diffIndex Git.Ref.headRef
			if null diff
				then void (liftIO cleanup) >> return True
				else void (liftIO cleanup) >> return False
		, return False
		)

start :: FilePath -> Key -> CommandStart
start file key = stopUnless (inAnnex key) $ do
	showStart "unannex" file
	next $ ifM isDirect
		( performDirect file key
		, performIndirect file key)

performIndirect :: FilePath -> Key -> CommandPerform
performIndirect file key = do
	liftIO $ removeFile file
	inRepo $ Git.Command.run
		[ Param "rm"
		, Param "--cached"
		, Param "--force"
		, Param "--quiet"
		, Param "--"
		, File file
		]
	next $ cleanupIndirect file key

cleanupIndirect :: FilePath -> Key -> CommandCleanup
cleanupIndirect file key = do
	Database.Keys.removeAssociatedFile key file
	src <- calcRepo $ gitAnnexLocation key
	ifM (Annex.getState Annex.fast)
		( do
			-- Only make a hard link if the annexed file does not
			-- already have other hard links pointing at it.
			-- This avoids unannexing (and uninit) ending up
			-- hard linking files together, which would be
			-- surprising.
			s <- liftIO $ getFileStatus src
			if linkCount s > 1
				then copyfrom src
				else hardlinkfrom src
		, copyfrom src
		)
  where
	copyfrom src = 
		thawContent file `after` liftIO (copyFileExternal CopyAllMetaData src file)
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
	inRepo $ Git.Command.run
		[ Param "rm"
		, Param "--cached"
		, Param "--force"
		, Param "--quiet"
		, Param "--"
		, File file
		]
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

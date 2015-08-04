{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Proxy where

import Common.Annex
import Command
import Config
import Utility.Tmp
import Utility.Env
import Annex.Direct
import qualified Git
import qualified Git.Sha
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.LsFiles
import Git.FilePath
import Utility.CopyFile

cmd :: Command
cmd = notBareRepo $
	command "proxy" SectionPlumbing 
		"safely bypass direct mode guard"
		("-- git command") (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = error "Did not specify command to run."
start (c:ps) = liftIO . exitWith =<< ifM isDirect
	( do
		tmp <- gitAnnexTmpMiscDir <$> gitRepo
		withTmpDirIn tmp "proxy" go
	, liftIO $ safeSystem c (map Param ps)
	)
  where
	go tmp = do
		oldref <- fromMaybe Git.Sha.emptyTree
			<$> inRepo Git.Ref.headSha
		
		setuptmpworktree tmp
		exitcode <- proxy tmp
		cleanupproxy tmp oldref

		return exitcode
	
	proxy tmp = do
		usetmp <- liftIO $ Just . addEntry "GIT_WORK_TREE" tmp  <$> getEnvironment
		unlessM (isNothing <$> inRepo Git.Branch.current) $
			unlessM (liftIO $ boolSystemEnv "git" [Param "checkout", Param "--", Param "."] usetmp) $
				error "Failed to set up proxy work tree."
		liftIO $ safeSystemEnv c (map Param ps) usetmp

	-- Commands like git revert will fail if there's a file
	-- in the work tree, or index, that would be overwritten
	-- by the revert. We want that to also happen when such a command
	-- is proxied.
	--
	-- It suffices to find any files in the real work tree that
	-- are not in the index, and hard link (or copy) them
	-- into the tmp work tree. This assumes that files that are in the
	-- index don't need to appear in the tmp work tree.
	setuptmpworktree tmp = do
		top <- fromRepo Git.repoPath
		(fs, cleanup) <- inRepo $ Git.LsFiles.notInRepo True [top]
		forM_ fs $ \f -> do
			tf <- inRepo $ toTopFilePath f
			let tmpf = tmp </> getTopFilePath tf
			liftIO $ do
				createDirectoryIfMissing True (takeDirectory tmpf)
				createLinkOrCopy f tmpf
		liftIO $ void cleanup
		
	-- To merge the changes made by the proxied command into
	-- the work tree is similar to cleaning up after a
	-- direct mode merge. But, here we force updates of any
	-- non-annxed files that were changed by the proxied
	-- command.
	cleanupproxy tmp oldref = do
		updateWorkTree tmp oldref True
		liftIO $ removeDirectoryRecursive tmp

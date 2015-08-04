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
import qualified Annex
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
			<$> (inRepo . maybe Git.Ref.headSha Git.Ref.sha
				=<< inRepo Git.Branch.currentUnsafe)
		
		exitcode <- proxy tmp

		cleanupproxy tmp oldref

		return exitcode
	
	proxy tmp = do
		usetmp <- liftIO $ Just . addEntry "GIT_WORK_TREE" tmp  <$> getEnvironment

		-- Set up the tmp work tree, to contain both a checkout of all
		-- staged files as well as hard links (or copies) of any
		-- unstaged files.
		unlessM (isNothing <$> inRepo Git.Branch.current) $
			unlessM (liftIO $ boolSystemEnv "git" [Param "checkout", Param "--", Param "."] usetmp) $
				error "Failed to set up proxy work tree."
		top <- fromRepo Git.repoPath
		(fs, cleanup) <- inRepo $ Git.LsFiles.notInRepo True [top]
		forM_ fs $ \f -> do
			tf <- inRepo $ toTopFilePath f
			let tmpf = tmp </> getTopFilePath tf
			liftIO $ do
				createDirectoryIfMissing True (takeDirectory tmpf)
				createLinkOrCopy f tmpf
		liftIO $ void cleanup

		liftIO $ safeSystemEnv c (map Param ps) usetmp
		
	-- To merge the changes made by the proxied command into
	-- the work tree is similar to cleaning up after a
	-- direct mode merge. But, here we force updates of any
	-- non-annxed files that were changed by the proxied
	-- command.
	cleanupproxy tmp oldref = do
		updateWorkTree tmp oldref True
		liftIO $ removeDirectoryRecursive tmp

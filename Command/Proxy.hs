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
import qualified Git.Sha
import qualified Git.Ref
import qualified Git.Branch

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
		exitcode <- proxy tmp
		mergeDirectCleanup tmp oldref
		return exitcode
	proxy tmp = do
		usetmp <- liftIO $ Just . addEntry "GIT_WORK_TREE" tmp  <$> getEnvironment
		unlessM (isNothing <$> inRepo Git.Branch.current) $
			unlessM (liftIO $ boolSystemEnv "git" [Param "checkout", Param "--", Param "."] usetmp) $
				error "Failed to set up proxy work tree."
		liftIO $ safeSystemEnv c (map Param ps) usetmp

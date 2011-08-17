{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

import Control.Monad.State (liftIO)
import System.Directory
import System.Exit

import Command
import Utility
import qualified Git
import qualified Annex
import qualified Command.Unannex
import Init
import qualified Branch
import Content
import Locations

command :: [Command]
command = [repoCommand "uninit" paramPath seek 
        "de-initialize git-annex and clean out repository"]

seek :: [CommandSeek]
seek = [withFilesInGit startUnannex, withNothing start]

startUnannex :: CommandStartString
startUnannex file = do
	-- Force fast mode before running unannex. This way, if multiple
	-- files link to a key, it will be left in the annex and hardlinked
	-- to by each.
	Annex.changeState $ \s -> s { Annex.fast = True }
	Command.Unannex.start file

start :: CommandStartNothing
start = next perform

perform :: CommandPerform
perform = next cleanup

cleanup :: CommandCleanup
cleanup = do
	g <- Annex.gitRepo
	uninitialize
	mapM_ removeAnnex =<< getKeysPresent
	liftIO $ removeDirectoryRecursive (gitAnnexDir g)
	-- avoid normal shutdown
	saveState
	liftIO $ do
		Git.run g "branch" [Param "-D", Param Branch.name]
		exitSuccess

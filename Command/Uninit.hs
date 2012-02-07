{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

import qualified Data.ByteString.Lazy.Char8 as B

import Common.Annex
import Command
import qualified Git
import qualified Git.Command
import qualified Annex
import qualified Command.Unannex
import Init
import qualified Annex.Branch
import Annex.Content

def :: [Command]
def = [addCheck check $ command "uninit" paramPaths seek 
        "de-initialize git-annex and clean out repository"]

check :: Annex ()
check = do
	b <- current_branch	
	when (b == Annex.Branch.name) $ error $
		"cannot uninit when the " ++ show b ++ " branch is checked out"
	where
		current_branch = Git.Ref . Prelude.head . lines . B.unpack <$> revhead
		revhead = inRepo $ Git.Command.pipeRead 
			[Params "rev-parse --abbrev-ref HEAD"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed startUnannex, withNothing start]

startUnannex :: FilePath -> (Key, Backend) -> CommandStart
startUnannex file info = do
	-- Force fast mode before running unannex. This way, if multiple
	-- files link to a key, it will be left in the annex and hardlinked
	-- to by each.
	Annex.changeState $ \s -> s { Annex.fast = True }
	Command.Unannex.start file info

start :: CommandStart
start = next perform

perform :: CommandPerform
perform = next cleanup

cleanup :: CommandCleanup
cleanup = do
	annexdir <- fromRepo gitAnnexDir
	uninitialize
	mapM_ removeAnnex =<< getKeysPresent
	liftIO $ removeDirectoryRecursive annexdir
	-- avoid normal shutdown
	saveState False
	inRepo $ Git.Command.run "branch"
		[Param "-D", Param $ show Annex.Branch.name]
	liftIO exitSuccess

{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Uninit where

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
def = [notDirect $ addCheck check $ command "uninit" paramPaths seek 
	"de-initialize git-annex and clean out repository"]

check :: Annex ()
check = do
	b <- current_branch
	when (b == Annex.Branch.name) $ error $
		"cannot uninit when the " ++ show b ++ " branch is checked out"
	top <- fromRepo Git.repoPath
	cwd <- liftIO getCurrentDirectory
	whenM ((/=) <$> liftIO (absPath top) <*> liftIO (absPath cwd)) $
		error "can only run uninit from the top of the git repository"
  where
	current_branch = Git.Ref . Prelude.head . lines <$> revhead
	revhead = inRepo $ Git.Command.pipeReadStrict
		[Params "rev-parse --abbrev-ref HEAD"]

seek :: [CommandSeek]
seek = 
	[ withFilesNotInGit $ whenAnnexed startCheckIncomplete
	, withFilesInGit $ whenAnnexed startUnannex
	, withNothing start
	]

{- git annex symlinks that are not checked into git could be left by an
 - interrupted add. -}
startCheckIncomplete :: FilePath -> (Key, Backend) -> CommandStart
startCheckIncomplete file _ = error $ unlines
	[ file ++ " points to annexed content, but is not checked into git."
	, "Perhaps this was left behind by an interrupted git annex add?"
	, "Not continuing with uninit; either delete or git annex add the file and retry."
	]

startUnannex :: FilePath -> (Key, Backend) -> CommandStart
startUnannex file info = do
	-- Force fast mode before running unannex. This way, if multiple
	-- files link to a key, it will be left in the annex and hardlinked
	-- to by each.
	Annex.changeState $ \s -> s { Annex.fast = True }
	Command.Unannex.start file info

start :: CommandStart
start = next $ next $ do
	annexdir <- fromRepo gitAnnexDir
	uninitialize
	mapM_ removeAnnex =<< getKeysPresent
	liftIO $ removeDirectoryRecursive annexdir
	-- avoid normal shutdown
	saveState False
	inRepo $ Git.Command.run "branch"
		[Param "-D", Param $ show Annex.Branch.name]
	liftIO exitSuccess

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
import qualified Annex
import qualified Command.Unannex
import Init
import qualified Annex.Branch
import Annex.Content

command :: [Command]
command = [Command "uninit" paramPaths check seek 
        "de-initialize git-annex and clean out repository"]

check :: Annex ()
check = do
	needsRepo
	b <- current_branch	
	when (b == Annex.Branch.name) $ error $
		"cannot uninit when the " ++ b ++ " branch is checked out"
	where
		current_branch = do
			g <- gitRepo
			b <- liftIO $
				Git.pipeRead g [Params "rev-parse --abbrev-ref HEAD"]
			return $ head $ lines $ B.unpack b

seek :: [CommandSeek]
seek = [withFilesInGit startUnannex, withNothing start]

startUnannex :: FilePath -> CommandStart
startUnannex file = do
	-- Force fast mode before running unannex. This way, if multiple
	-- files link to a key, it will be left in the annex and hardlinked
	-- to by each.
	Annex.changeState $ \s -> s { Annex.fast = True }
	Command.Unannex.start file

start :: CommandStart
start = next perform

perform :: CommandPerform
perform = next cleanup

cleanup :: CommandCleanup
cleanup = do
	g <- gitRepo
	uninitialize
	mapM_ removeAnnex =<< getKeysPresent
	liftIO $ removeDirectoryRecursive (gitAnnexDir g)
	-- avoid normal shutdown
	saveState
	liftIO $ do
		Git.run g "branch" [Param "-D", Param Annex.Branch.name]
		exitSuccess

{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Find where

import Control.Monad.State (liftIO)

import Command
import Content
import Utility.Conditional

command :: [Command]
command = [repoCommand "find" (paramOptional $ paramRepeating paramPath) seek
	"lists available files"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- Output a list of files. -}
start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	whenM (inAnnex key) $ liftIO $ putStrLn file
	stop

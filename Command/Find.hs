{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Find where

import Common.Annex
import Command
import Annex.Content
import Limit
import qualified Annex

def :: [Command]
def = [command "find" paramPaths seek "lists available files"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend Annex) -> CommandStart
start file (key, _) = do
	-- only files inAnnex are shown, unless the user has requested
	-- others via a limit
	whenM (liftM2 (||) (inAnnex key) limited) $ do
		print0 <- Annex.getState Annex.print0
		if print0
			then liftIO $ putStr (file ++ "\0")
			else liftIO $ putStrLn file
	stop

{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Find where

import qualified Data.Map as M

import Common.Annex
import Command
import Annex.Content
import Limit
import qualified Annex
import qualified Utility.Format
import Utility.DataUnits
import Types.Key

def :: [Command]
def = [command "find" paramPaths seek "lists available files"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend Annex) -> CommandStart
start file (key, _) = do
	-- only files inAnnex are shown, unless the user has requested
	-- others via a limit
	whenM (liftM2 (||) (inAnnex key) limited) $ do
		f <- Annex.getState Annex.format
		case f of
			Nothing -> liftIO $ putStrLn file
			Just formatter -> liftIO $ putStr $
				Utility.Format.format formatter vars
	stop
	where
		vars = M.fromList
			[ ("file", file)
			, ("key", show key)
			, ("backend", keyBackendName key)
			, ("bytesize", size show)
			, ("humansize", size $ roughSize storageUnits True)
			]
		size c = maybe "unknown" c $ keySize key

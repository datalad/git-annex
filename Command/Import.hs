{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Import where

import Common.Annex
import Command
import qualified Annex
import qualified Command.Add

def :: [Command]
def = [command "import" paramPaths seek "move and add files from outside git working copy"]

seek :: [CommandSeek]
seek = [withPathContents start]

start :: (FilePath, FilePath) -> CommandStart
start (srcfile, destfile) = notBareRepo $
	ifM (liftIO $ isRegularFile <$> getSymbolicLinkStatus srcfile)
		( do
			showStart "import" destfile
			next $ perform srcfile destfile
		, stop
		)

perform :: FilePath -> FilePath -> CommandPerform
perform srcfile destfile = do
	whenM (liftIO $ doesFileExist destfile) $
		unlessM (Annex.getState Annex.force) $
			error $ "not overwriting existing " ++ destfile ++
				" (use --force to override)"
 
	liftIO $ createDirectoryIfMissing True (parentDir destfile)
	liftIO $ moveFile srcfile destfile
	Command.Add.perform destfile

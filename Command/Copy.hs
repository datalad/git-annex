{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Command
import qualified Command.Move

command :: [Command]
command = [repoCommand "copy" paramPaths seek
	"copy content of files to/from another repository"]

-- A copy is just a move that does not delete the source file.
seek :: [CommandSeek]
seek = [withFilesInGit $ Command.Move.start False]

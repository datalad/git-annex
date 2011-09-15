{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Command
import qualified Command.Move
import Utility

command :: [Command]
command = [repoCommand "copy" paramPaths seek
	"copy content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withNumCopies start]

-- A copy is just a move that does not delete the source file.
-- However, --auto mode avoids unnecessary copies.
start :: CommandStartAttrFile
start (file, attr) = isAnnexed file $ \(key, _) ->
	autoCopies key (<) numcopies $
		Command.Move.start False file
	where
		numcopies = readMaybe attr

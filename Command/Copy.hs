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
command = [Command "copy" paramPaths needsRepo seek
	"copy content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withNumCopies start]

-- A copy is just a move that does not delete the source file.
-- However, --auto mode avoids unnecessary copies.
start :: FilePath -> Maybe Int -> CommandStart
start file numcopies = isAnnexed file $ \(key, _) ->
	autoCopies key (<) numcopies $
		Command.Move.start False file

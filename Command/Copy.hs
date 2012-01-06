{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Copy where

import Common.Annex
import Command
import qualified Command.Move
import qualified Remote

def :: [Command]
def = [withOptions Command.Move.options $ command "copy" paramPaths seek
	"copy content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withField Command.Move.toOption Remote.byName $ \to ->
		withField Command.Move.fromOption Remote.byName $ \from ->
			withNumCopies $ \n -> whenAnnexed $ start to from n]

-- A copy is just a move that does not delete the source file.
-- However, --auto mode avoids unnecessary copies.
start :: Maybe Remote -> Maybe Remote -> Maybe Int -> FilePath -> (Key, Backend) -> CommandStart
start to from numcopies file (key, backend) = autoCopies key (<) numcopies $
	Command.Move.start to from False file (key, backend)

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

def :: [Command]
def = [dontCheck toOpt $ dontCheck fromOpt $ 
	command "copy" paramPaths seek
	"copy content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withNumCopies $ \n -> whenAnnexed $ start n]

-- A copy is just a move that does not delete the source file.
-- However, --auto mode avoids unnecessary copies.
start :: Maybe Int -> FilePath -> (Key, Backend) -> CommandStart
start numcopies file (key, backend) = autoCopies key (<) numcopies $
	Command.Move.start False file (key, backend)

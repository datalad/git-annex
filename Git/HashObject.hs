{- git hash-object interface
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.HashObject where

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import qualified Utility.CoProcess as CoProcess

type HashObjectHandle = CoProcess.CoProcessHandle

hashObjectStart :: Repo -> IO HashObjectHandle
hashObjectStart = CoProcess.start "git" . toCommand . gitCommandLine
	[ Param "hash-object"
	, Param "-w"
	, Param "--stdin-paths"
	]

hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop = CoProcess.stop

{- Injects a file into git, returning the Sha of the object. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
	where
		send to = do
			fileEncoding to
			hPutStrLn to file
		receive from = getSha "hash-object" $ hGetLine from

{- Injects some content into git, returning its Sha. -}
hashObject :: ObjectType -> String -> Repo -> IO Sha
hashObject objtype content repo = getSha subcmd $ do
	s <- pipeWriteRead (map Param params) content repo
	reap -- XXX unsure why this is needed, of if it is anymore
	return s
	where
		subcmd = "hash-object"
		params = [subcmd, "-t", show objtype, "-w", "--stdin"]

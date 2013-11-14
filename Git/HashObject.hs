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
hashObjectStart = CoProcess.rawMode <=< gitCoProcessStart True
	[ Param "hash-object"
	, Param "-w"
	, Param "--stdin-paths"
	, Param "--no-filters"
	]

hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop = CoProcess.stop

{- Injects a file into git, returning the Sha of the object. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
  where
	send to = hPutStrLn to file
	receive from = getSha "hash-object" $ hGetLine from

{- Injects some content into git, returning its Sha. -}
hashObject :: ObjectType -> String -> Repo -> IO Sha
hashObject objtype content = hashObject' objtype (flip hPutStr content)

hashObject' :: ObjectType -> (Handle -> IO ()) -> Repo -> IO Sha
hashObject' objtype writer repo = getSha subcmd $
	pipeWriteRead (map Param params) (Just writer) repo
  where
	subcmd = "hash-object"
	params = [subcmd, "-t", show objtype, "-w", "--stdin", "--no-filters"]

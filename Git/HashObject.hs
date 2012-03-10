{- git hash-object interface
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.HashObject where

import Common
import Git
import Git.Command
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

{- Injects a file into git, returning the shas of the objects. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
	where
		send to = do
			fileEncoding to
			hPutStrLn to file
		receive from = Ref <$> hGetLine from

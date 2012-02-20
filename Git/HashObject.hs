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

{- Starts git hash-object and returns a handle.  -}
hashObjectStart :: Repo -> IO HashObjectHandle
hashObjectStart repo = do
	h <- CoProcess.start "git" $ toCommand $ gitCommandLine params repo
	CoProcess.query h fileEncoding (const $ return ())
	return h
	where
		params =
			[ Param "hash-object"
			, Param "-w"
			, Param "--stdin-paths"
			]

{- Stops git hash-object. -}
hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop = CoProcess.stop

{- Injects a file into git, returning the shas of the objects. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
	where
		send to = hPutStrLn to file
		receive from = Ref <$> hGetLine from

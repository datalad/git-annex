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

type HashObjectHandle = (PipeHandle, Handle, Handle)

{- Starts git hash-object and returns a handle.  -}
hashObjectStart :: Repo -> IO HashObjectHandle
hashObjectStart repo = do
	r@(_, _, toh) <- hPipeBoth "git" $
		toCommand $ gitCommandLine params repo
	fileEncoding toh
	return r
	where
		params =
			[ Param "hash-object"
			, Param "-w"
			, Param "--stdin-paths"
			]

{- Stops git hash-object. -}
hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop (pid, from, to) = do
	hClose to
	hClose from
	forceSuccess pid

{- Injects a file into git, returning the shas of the objects. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile (_, from, to) file = do
	hPutStrLn to file
	hFlush to
	Ref <$> hGetLine from

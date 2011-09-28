{- git cat-file interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CatFile (
	CatFileHandle,
	catFileStart,
	catFileStop,
	catFile
) where

import Control.Monad.State
import System.Cmd.Utils
import System.IO
import qualified Data.ByteString.Char8 as B

import Git
import Utility.SafeCommand

type CatFileHandle = (PipeHandle, Handle, Handle)

{- Starts git cat-file running in batch mode in a repo and returns a handle. -}
catFileStart :: Repo -> IO CatFileHandle
catFileStart repo = hPipeBoth "git" $ toCommand $
	Git.gitCommandLine repo [Param "cat-file", Param "--batch"]

{- Stops git cat-file. -}
catFileStop :: CatFileHandle -> IO ()
catFileStop (pid, from, to) = do
	hClose to
	hClose from
	forceSuccess pid

{- Uses a running git cat-file read the content of a file from a branch.
 - Files that do not exist on the branch will have "" returned. -}
catFile :: CatFileHandle -> String -> FilePath -> IO String
catFile (_, from, to) branch file = do
	hPutStrLn to want
	hFlush to
	header <- hGetLine from
	case words header of
		[sha, blob, size]
			| length sha == Git.shaSize &&
			  blob == "blob" -> handle size
			| otherwise -> empty
		_
			| header == want ++ " missing" -> empty
			| otherwise -> error $ "unknown response from git cat-file " ++ header
	where
		want = branch ++ ":" ++ file
		handle size = case reads size of
			[(bytes, "")] -> readcontent bytes
			_ -> empty
		readcontent bytes = do
			content <- B.hGet from bytes
			c <- hGetChar from
			when (c /= '\n') $
				error "missing newline from git cat-file"
			return $ B.unpack content
		empty = return ""

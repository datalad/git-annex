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
	catFile,
	catObject
) where

import System.Cmd.Utils
import System.IO
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Common
import Git
import Git.Sha
import Git.Command

type CatFileHandle = (PipeHandle, Handle, Handle)

{- Starts git cat-file running in batch mode in a repo and returns a handle. -}
catFileStart :: Repo -> IO CatFileHandle
catFileStart repo = hPipeBoth "git" $ toCommand $
	gitCommandLine [Param "cat-file", Param "--batch"] repo

{- Stops git cat-file. -}
catFileStop :: CatFileHandle -> IO ()
catFileStop (pid, from, to) = do
	hClose to
	hClose from
	forceSuccess pid

{- Reads a file from a specified branch. -}
catFile :: CatFileHandle -> Branch -> FilePath -> IO L.ByteString
catFile h branch file = catObject h $ Ref $ show branch ++ ":" ++ file

{- Uses a running git cat-file read the content of an object.
 - Objects that do not exist will have "" returned. -}
catObject :: CatFileHandle -> Ref -> IO L.ByteString
catObject (_, from, to) object = do
	hPutStrLn to $ show object
	hFlush to
	header <- hGetLine from
	case words header of
		[sha, objtype, size]
			| length sha == shaSize &&
			  validobjtype objtype -> handle size
			| otherwise -> dne
		_
			| header == show object ++ " missing" -> dne
			| otherwise -> error $ "unknown response from git cat-file " ++ header
	where
		handle size = case reads size of
			[(bytes, "")] -> readcontent bytes
			_ -> dne
		readcontent bytes = do
			content <- S.hGet from bytes
			c <- hGetChar from
			when (c /= '\n') $
				error "missing newline from git cat-file"
			return $ L.fromChunks [content]
		dne = return L.empty
		validobjtype t
			| t == "blob" = True
			| t == "commit" = True
			| t == "tree" = True
			| otherwise = False

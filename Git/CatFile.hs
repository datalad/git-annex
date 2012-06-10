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

import System.IO
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import qualified Utility.CoProcess as CoProcess

type CatFileHandle = CoProcess.CoProcessHandle

catFileStart :: Repo -> IO CatFileHandle
catFileStart = CoProcess.start "git" . toCommand . gitCommandLine
	[ Param "cat-file"
	, Param "--batch"
	]

catFileStop :: CatFileHandle -> IO ()
catFileStop = CoProcess.stop

{- Reads a file from a specified branch. -}
catFile :: CatFileHandle -> Branch -> FilePath -> IO L.ByteString
catFile h branch file = catObject h $ Ref $ show branch ++ ":" ++ file

{- Uses a running git cat-file read the content of an object.
 - Objects that do not exist will have "" returned. -}
catObject :: CatFileHandle -> Ref -> IO L.ByteString
catObject h object = CoProcess.query h send receive
	where
		send to = do
			fileEncoding to
			hPutStrLn to $ show object
		receive from = do
			fileEncoding from
			header <- hGetLine from
			case words header of
				[sha, objtype, size]
					| length sha == shaSize &&
					  isJust (readObjectType objtype) -> 
						case reads size of
							[(bytes, "")] -> readcontent bytes from
							_ -> dne
					| otherwise -> dne
				_
					| header == show object ++ " missing" -> dne
					| otherwise -> error $ "unknown response from git cat-file " ++ show (header, object)
		readcontent bytes from = do
			content <- S.hGet from bytes
			c <- hGetChar from
			when (c /= '\n') $
				error "missing newline from git cat-file"
			return $ L.fromChunks [content]
		dne = return L.empty

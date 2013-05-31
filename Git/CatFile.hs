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
	catObject,
	catObjectDetails,
) where

import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import Git.FilePath
import qualified Utility.CoProcess as CoProcess

type CatFileHandle = CoProcess.CoProcessHandle

catFileStart :: Repo -> IO CatFileHandle
catFileStart = CoProcess.rawMode <=< gitCoProcessStart True
	[ Param "cat-file"
	, Param "--batch"
	]

catFileStop :: CatFileHandle -> IO ()
catFileStop = CoProcess.stop

{- Reads a file from a specified branch. -}
catFile :: CatFileHandle -> Branch -> FilePath -> IO L.ByteString
catFile h branch file = catObject h $ Ref $
	show branch ++ ":" ++ toInternalGitPath file

{- Uses a running git cat-file read the content of an object.
 - Objects that do not exist will have "" returned. -}
catObject :: CatFileHandle -> Ref -> IO L.ByteString
catObject h object = maybe L.empty fst <$> catObjectDetails h object

{- Gets both the content of an object, and its Sha. -}
catObjectDetails :: CatFileHandle -> Ref -> IO (Maybe (L.ByteString, Sha))
catObjectDetails h object = CoProcess.query h send receive
  where
	send to = hPutStrLn to $ show object
	receive from = do
		header <- hGetLine from
		case words header of
			[sha, objtype, size]
				| length sha == shaSize &&
				  isJust (readObjectType objtype) -> 
					case reads size of
						[(bytes, "")] -> readcontent bytes from sha
						_ -> dne
				| otherwise -> dne
			_
				| header == show object ++ " missing" -> dne
				| otherwise -> error $ "unknown response from git cat-file " ++ show (header, object)
	readcontent bytes from sha = do
		content <- S.hGet from bytes
		eatchar '\n' from
		return $ Just (L.fromChunks [content], Ref sha)
	dne = return Nothing
	eatchar expected from = do
		c <- hGetChar from
		when (c /= expected) $
			error $ "missing " ++ (show expected) ++ " from git cat-file"

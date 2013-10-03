{- git cat-file interface
 -
 - Copyright 2011, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CatFile (
	CatFileHandle,
	catFileStart,
	catFileStop,
	catFile,
	catTree,
	catObject,
	catObjectDetails,
) where

import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.SHA
import Data.Char
import System.Process (std_out, std_err)
import Numeric
import System.Posix.Types

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import Git.FilePath
import qualified Utility.CoProcess as CoProcess

data CatFileHandle = CatFileHandle CoProcess.CoProcessHandle Repo

catFileStart :: Repo -> IO CatFileHandle
catFileStart repo = do
	coprocess <- CoProcess.rawMode =<< gitCoProcessStart True
		[ Param "cat-file"
		, Param "--batch"
		] repo
	return $ CatFileHandle coprocess repo

catFileStop :: CatFileHandle -> IO ()
catFileStop (CatFileHandle p _) = CoProcess.stop p

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
catObjectDetails (CatFileHandle hdl repo) object = CoProcess.query hdl send receive
  where
	query = show object
	send to = hPutStrLn to query
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
				| otherwise -> 
					if any isSpace query
						then fallback
						else error $ "unknown response from git cat-file " ++ show (header, object)
	readcontent bytes from sha = do
		content <- S.hGet from bytes
		eatchar '\n' from
		return $ Just (L.fromChunks [content], Ref sha)
	dne = return Nothing
	eatchar expected from = do
		c <- hGetChar from
		when (c /= expected) $
			error $ "missing " ++ (show expected) ++ " from git cat-file"

	{- Work around a bug in git 1.8.4 rc0 which broke it for filenames
	 - containing spaces. http://bugs.debian.org/718517   
	 - Slow! Also can use a lot of memory, if the object is large. -}
	fallback = do
		let p = gitCreateProcess 
			[ Param "cat-file"
			, Param "-p"
			, Param query
			] repo
		(_, Just h, _, pid) <- withNullHandle $ \h -> 
			createProcess p
				{ std_out = CreatePipe
				, std_err = UseHandle h
				}
		fileEncoding h
		content <- L.hGetContents h
		let sha = (\s -> length s `seq` s) (showDigest $ sha1 content)
		ok <- checkSuccessProcess pid
		return $ if ok
			then Just (content, Ref sha)
			else Nothing

{- Gets a list of files and directories in a tree. (Not recursive.) -}
catTree :: CatFileHandle -> Ref -> IO [(FilePath, FileMode)]
catTree h treeref = go <$> catObjectDetails h treeref
  where
  	go Nothing = []
	go (Just (b, _)) = parsetree [] b

	parsetree c b = case L.break (== 0) b of
		(modefile, rest)
			| L.null modefile -> c
			| otherwise -> parsetree
				(parsemodefile modefile:c)
				(dropsha rest)

	-- these 20 bytes after the NUL hold the file's sha
	-- TODO: convert from raw form to regular sha
	dropsha = L.drop 21

	parsemodefile b = 
		let (modestr, file) = separate (== ' ') (encodeW8 $ L.unpack b)
		in (file, readmode modestr)
	readmode = fst . fromMaybe (0, undefined) . headMaybe . readOct

{- git cat-file interface
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CatFile (
	CatFileHandle,
	catFileStart,
	catFileStart',
	catFileStop,
	catFile,
	catFileDetails,
	catTree,
	catCommit,
	catObject,
	catObjectDetails,
) where

import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as M
import Data.String
import Data.Char
import Data.Tuple.Utils
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
catFileStart = catFileStart' True

catFileStart' :: Bool -> Repo -> IO CatFileHandle
catFileStart' restartable repo = do
	coprocess <- CoProcess.rawMode =<< gitCoProcessStart restartable
		[ Param "cat-file"
		, Param "--batch"
		] repo
	return $ CatFileHandle coprocess repo

catFileStop :: CatFileHandle -> IO ()
catFileStop (CatFileHandle p _) = CoProcess.stop p

{- Reads a file from a specified branch. -}
catFile :: CatFileHandle -> Branch -> FilePath -> IO L.ByteString
catFile h branch file = catObject h $ Ref $
	fromRef branch ++ ":" ++ toInternalGitPath file

catFileDetails :: CatFileHandle -> Branch -> FilePath -> IO (Maybe (L.ByteString, Sha, ObjectType))
catFileDetails h branch file = catObjectDetails h $ Ref $
	fromRef branch ++ ":" ++ toInternalGitPath file

{- Uses a running git cat-file read the content of an object.
 - Objects that do not exist will have "" returned. -}
catObject :: CatFileHandle -> Ref -> IO L.ByteString
catObject h object = maybe L.empty fst3 <$> catObjectDetails h object

catObjectDetails :: CatFileHandle -> Ref -> IO (Maybe (L.ByteString, Sha, ObjectType))
catObjectDetails (CatFileHandle hdl _) object = CoProcess.query hdl send receive
  where
	query = fromRef object
	send to = hPutStrLn to query
	receive from = do
		header <- hGetLine from
		case words header of
			[sha, objtype, size]
				| length sha == shaSize ->
					case (readObjectType objtype, reads size) of
						(Just t, [(bytes, "")]) -> readcontent t bytes from sha
						_ -> dne
				| otherwise -> dne
			_
				| header == fromRef object ++ " missing" -> dne
				| otherwise -> error $ "unknown response from git cat-file " ++ show (header, query)
	readcontent objtype bytes from sha = do
		content <- S.hGet from bytes
		eatchar '\n' from
		return $ Just (L.fromChunks [content], Ref sha, objtype)
	dne = return Nothing
	eatchar expected from = do
		c <- hGetChar from
		when (c /= expected) $
			error $ "missing " ++ (show expected) ++ " from git cat-file"

{- Gets a list of files and directories in a tree. (Not recursive.) -}
catTree :: CatFileHandle -> Ref -> IO [(FilePath, FileMode)]
catTree h treeref = go <$> catObjectDetails h treeref
  where
	go (Just (b, _, TreeObject)) = parsetree [] b
	go _ = []

	parsetree c b = case L.break (== 0) b of
		(modefile, rest)
			| L.null modefile -> c
			| otherwise -> parsetree
				(parsemodefile modefile:c)
				(dropsha rest)

	-- these 20 bytes after the NUL hold the file's sha
	dropsha = L.drop 21

	parsemodefile b = 
		let (modestr, file) = separate (== ' ') (decodeBS b)
		in (file, readmode modestr)
	readmode = fromMaybe 0 . fmap fst . headMaybe . readOct

catCommit :: CatFileHandle -> Ref -> IO (Maybe Commit)
catCommit h commitref = go <$> catObjectDetails h commitref
  where
	go (Just (b, _, CommitObject)) = parseCommit b
	go _ = Nothing

parseCommit :: L.ByteString -> Maybe Commit
parseCommit b = Commit
	<$> (extractSha . L8.unpack =<< field "tree")
	<*> (mapMaybe (extractSha . L8.unpack) <$> fields "parent")
	<*> (parsemetadata <$> field "author")
	<*> (parsemetadata <$> field "committer")
	<*> Just (L8.unpack $ L.intercalate (L.singleton nl) message)
  where
	field n = headMaybe =<< fields n
	fields n = M.lookup (fromString n) fieldmap
	fieldmap = M.fromListWith (++) ((map breakfield) header)
	breakfield l =
		let (k, sp_v) = L.break (== sp) l
		in (k, [L.drop 1 sp_v])
	(header, message) = separate L.null ls
	ls = L.split nl b

	-- author and committer lines have the form: "name <email> date"
	-- The email is always present, even if empty "<>"
	parsemetadata l = CommitMetaData
		{ commitName = whenset $ L.init name_sp
		, commitEmail = whenset email
		, commitDate = whenset $ L.drop 2 gt_sp_date
		}
	  where
		(name_sp, rest) = L.break (== lt) l
		(email, gt_sp_date) = L.break (== gt) (L.drop 1 rest)
		whenset v
			| L.null v = Nothing
			| otherwise = Just (L8.unpack v)

	nl = fromIntegral (ord '\n')
	sp = fromIntegral (ord ' ')
	lt = fromIntegral (ord '<')
	gt = fromIntegral (ord '>')

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
	catObjectMetaData,
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

data CatFileHandle = CatFileHandle 
	{ catFileProcess :: CoProcess.CoProcessHandle
	, checkFileProcess :: CoProcess.CoProcessHandle
	}

catFileStart :: Repo -> IO CatFileHandle
catFileStart = catFileStart' True

catFileStart' :: Bool -> Repo -> IO CatFileHandle
catFileStart' restartable repo = CatFileHandle
	<$> startp "--batch"
	<*> startp "--batch-check=%(objectname) %(objecttype) %(objectsize)"
  where
	startp p = CoProcess.rawMode =<< gitCoProcessStart restartable
		[ Param "cat-file"
		, Param p
		] repo

catFileStop :: CatFileHandle -> IO ()
catFileStop h = do
	CoProcess.stop (catFileProcess h)
	CoProcess.stop (checkFileProcess h)

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
catObjectDetails h object = query (catFileProcess h) object $ \from -> do
	header <- hGetLine from
	case parseResp object header of
		Just (ParsedResp sha size objtype) -> do
			content <- S.hGet from (fromIntegral size)
			eatchar '\n' from
			return $ Just (L.fromChunks [content], sha, objtype)
		Just DNE -> return Nothing
		Nothing -> error $ "unknown response from git cat-file " ++ show (header, object)
  where
	eatchar expected from = do
		c <- hGetChar from
		when (c /= expected) $
			error $ "missing " ++ (show expected) ++ " from git cat-file"

{- Gets the size and type of an object, without reading its content. -}
catObjectMetaData :: CatFileHandle -> Ref -> IO (Maybe (Integer, ObjectType))
catObjectMetaData h object = query (checkFileProcess h) object $ \from -> do
	resp <- hGetLine from
	case parseResp object resp of
		Just (ParsedResp _ size objtype) ->
			return $ Just (size, objtype)
		Just DNE -> return Nothing
		Nothing -> error $ "unknown response from git cat-file " ++ show (resp, object)

data ParsedResp = ParsedResp Sha Integer ObjectType | DNE

query :: CoProcess.CoProcessHandle -> Ref -> (Handle -> IO a) -> IO a
query hdl object receive = CoProcess.query hdl send receive
  where
	send to = hPutStrLn to (fromRef object)

parseResp :: Ref -> String -> Maybe ParsedResp
parseResp object l = case words l of
	[sha, objtype, size]
		| length sha == shaSize ->
			case (readObjectType objtype, reads size) of
				(Just t, [(bytes, "")]) -> 
					Just $ ParsedResp (Ref sha) bytes t
				_ -> Nothing
		| otherwise -> Nothing
	_
		| l == fromRef object ++ " missing" -> Just DNE
		| otherwise -> Nothing

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
	<*> Just (maybe [] (mapMaybe (extractSha . L8.unpack)) (fields "parent"))
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

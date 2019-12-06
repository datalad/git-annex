{- git cat-file interface
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Numeric
import System.Posix.Types
import Text.Read

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import Git.FilePath
import Git.HashObject
import qualified Utility.CoProcess as CoProcess
import Utility.Tuple

data CatFileHandle = CatFileHandle 
	{ catFileProcess :: CoProcess.CoProcessHandle
	, checkFileProcess :: CoProcess.CoProcessHandle
	, gitRepo :: Repo
	}

catFileStart :: Repo -> IO CatFileHandle
catFileStart = catFileStart' True

catFileStart' :: Bool -> Repo -> IO CatFileHandle
catFileStart' restartable repo = CatFileHandle
	<$> startp "--batch"
	<*> startp "--batch-check=%(objectname) %(objecttype) %(objectsize)"
	<*> pure repo
  where
	startp p = gitCoProcessStart restartable
		[ Param "cat-file"
		, Param p
		] repo

catFileStop :: CatFileHandle -> IO ()
catFileStop h = do
	CoProcess.stop (catFileProcess h)
	CoProcess.stop (checkFileProcess h)

{- Reads a file from a specified branch. -}
catFile :: CatFileHandle -> Branch -> RawFilePath -> IO L.ByteString
catFile h branch file = catObject h $ Ref $
	fromRef branch ++ ":" ++ fromRawFilePath (toInternalGitPath file)

catFileDetails :: CatFileHandle -> Branch -> RawFilePath -> IO (Maybe (L.ByteString, Sha, ObjectType))
catFileDetails h branch file = catObjectDetails h $ Ref $
	fromRef branch ++ ":" ++ fromRawFilePath (toInternalGitPath file)

{- Uses a running git cat-file read the content of an object.
 - Objects that do not exist will have "" returned. -}
catObject :: CatFileHandle -> Ref -> IO L.ByteString
catObject h object = maybe L.empty fst3 <$> catObjectDetails h object

catObjectDetails :: CatFileHandle -> Ref -> IO (Maybe (L.ByteString, Sha, ObjectType))
catObjectDetails h object = query (catFileProcess h) object newlinefallback $ \from -> do
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
	
	-- Slow fallback path for filenames containing newlines.
	newlinefallback = queryObjectType object (gitRepo h) >>= \case
		Nothing -> return Nothing
		Just objtype -> queryContent object (gitRepo h) >>= \case
			Nothing -> return Nothing
			Just content -> do
				-- only the --batch interface allows getting
				-- the sha, so have to re-hash the object
				sha <- hashObject' objtype
					(flip L.hPut content)
					(gitRepo h)
				return (Just (content, sha, objtype))

{- Gets the size and type of an object, without reading its content. -}
catObjectMetaData :: CatFileHandle -> Ref -> IO (Maybe (Integer, ObjectType))
catObjectMetaData h object = query (checkFileProcess h) object newlinefallback $ \from -> do
	resp <- hGetLine from
	case parseResp object resp of
		Just (ParsedResp _ size objtype) ->
			return $ Just (size, objtype)
		Just DNE -> return Nothing
		Nothing -> error $ "unknown response from git cat-file " ++ show (resp, object)
  where
	-- Slow fallback path for filenames containing newlines.
	newlinefallback = do
		sz <- querySize object (gitRepo h)
		objtype <- queryObjectType object (gitRepo h)
		return $ (,) <$> sz <*> objtype

data ParsedResp = ParsedResp Sha Integer ObjectType | DNE

query :: CoProcess.CoProcessHandle -> Ref -> IO a -> (Handle -> IO a) -> IO a
query hdl object newlinefallback receive
	-- git cat-file --batch uses a line based protocol, so when the
	-- filename itself contains a newline, have to fall back to another
	-- method of getting the information.
	| '\n' `elem` s = newlinefallback
	-- git strips carriage return from the end of a line, out of some
	-- misplaced desire to support windows, so also use the newline
	-- fallback for those.
	| "\r" `isSuffixOf` s = newlinefallback
	| otherwise = CoProcess.query hdl send receive
  where
	send to = hPutStrLn to s
	s = fromRef object

parseResp :: Ref -> String -> Maybe ParsedResp
parseResp object l 
	| " missing" `isSuffixOf` l -- less expensive than full check
		&& l == fromRef object ++ " missing" = Just DNE
	| otherwise = case words l of
		[sha, objtype, size]
			| length sha == shaSize ->
				case (readObjectType (encodeBS objtype), reads size) of
					(Just t, [(bytes, "")]) -> 
						Just $ ParsedResp (Ref sha) bytes t
					_ -> Nothing
			| otherwise -> Nothing
		_ -> Nothing

querySingle :: CommandParam -> Ref -> Repo -> (Handle -> IO a) -> IO (Maybe a)
querySingle o r repo reader = assertLocal repo $
	-- In non-batch mode, git cat-file warns on stderr when
	-- asked for an object that does not exist.
	-- Squelch that warning to behave the same as batch mode.
	withNullHandle $ \nullh -> do
		let p = gitCreateProcess 
			[ Param "cat-file"
			, o
			, Param (fromRef r)
			] repo
		let p' = p
			{ std_err = UseHandle nullh
			, std_in = Inherit
			, std_out = CreatePipe
			}
		pid <- createProcess p'
		let h = stdoutHandle pid
		output <- reader h
		hClose h
		ifM (checkSuccessProcess (processHandle pid))
			( return (Just output)
			, return Nothing
			)

querySize :: Ref -> Repo -> IO (Maybe Integer)
querySize r repo = maybe Nothing (readMaybe . takeWhile (/= '\n'))
	<$> querySingle (Param "-s") r repo hGetContentsStrict

queryObjectType :: Ref -> Repo -> IO (Maybe ObjectType)
queryObjectType r repo = maybe Nothing (readObjectType . encodeBS . takeWhile (/= '\n'))
	<$> querySingle (Param "-t") r repo hGetContentsStrict

queryContent :: Ref -> Repo -> IO (Maybe L.ByteString)
queryContent r repo = fmap (\b -> L.fromChunks [b])
	<$> querySingle (Param "-p") r repo S.hGetContents

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
		let (modestr, file) = separate (== ' ') (decodeBL b)
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

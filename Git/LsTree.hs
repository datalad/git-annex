{- git ls-tree interface
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.LsTree (
	TreeItem(..),
	LsTreeMode(..),
	lsTree,
	lsTree',
	lsTreeStrict,
	lsTreeStrict',
	lsTreeParams,
	lsTreeFiles,
	parseLsTree,
	formatLsTree,
) where

import Common
import Git
import Git.Command
import Git.FilePath
import qualified Git.Filename
import Utility.Attoparsec

import Numeric
import Data.Either
import System.Posix.Types
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8

data TreeItem = TreeItem
	{ mode :: FileMode
	, typeobj :: S.ByteString
	, sha :: Ref
	, file :: TopFilePath
	} deriving (Show)

data LsTreeMode = LsTreeRecursive | LsTreeNonRecursive

{- Lists the contents of a tree, with lazy output. -}
lsTree :: LsTreeMode -> Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree = lsTree' []

lsTree' :: [CommandParam] -> LsTreeMode -> Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree' ps lsmode t repo = do
	(l, cleanup) <- pipeNullSplit (lsTreeParams lsmode t ps) repo
	return (rights (map parseLsTree l), cleanup)

lsTreeStrict :: LsTreeMode -> Ref -> Repo -> IO [TreeItem]
lsTreeStrict = lsTreeStrict' []

lsTreeStrict' :: [CommandParam] -> LsTreeMode -> Ref -> Repo -> IO [TreeItem]
lsTreeStrict' ps lsmode t repo = rights . map parseLsTreeStrict
	<$> pipeNullSplitStrict (lsTreeParams lsmode t ps) repo

lsTreeParams :: LsTreeMode -> Ref -> [CommandParam] -> [CommandParam]
lsTreeParams lsmode r ps =
	[ Param "ls-tree"
	, Param "--full-tree"
	, Param "-z"
	] ++ recursiveparams ++ ps ++
	[ Param "--"
	, File $ fromRef r
	]
  where
	recursiveparams = case lsmode of
		LsTreeRecursive -> [ Param "-r" ]
		LsTreeNonRecursive -> []

{- Lists specified files in a tree. -}
lsTreeFiles :: Ref -> [FilePath] -> Repo -> IO [TreeItem]
lsTreeFiles t fs repo = rights . map (parseLsTree . L.fromStrict)
	<$> pipeNullSplitStrict ps repo
  where
	ps =
		[ Param "ls-tree"
		, Param "--full-tree"
		, Param "-z"
		, Param "--"
		, File $ fromRef t
		] ++ map File fs

parseLsTree :: L.ByteString -> Either String TreeItem
parseLsTree b = case A.parse parserLsTree b of
	A.Done _ r  -> Right r
	A.Fail _ _ err -> Left err

parseLsTreeStrict :: S.ByteString -> Either String TreeItem
parseLsTreeStrict b = go (AS.parse parserLsTree b)
  where
	go (AS.Done _ r) = Right r
	go (AS.Fail _ _ err) = Left err
	go (AS.Partial c) = go (c mempty)

{- Parses a line of ls-tree output, in format:
 - mode SP type SP sha TAB file
 -
 - The TAB can also be a space. Git does not use that, but an earlier
 - version of formatLsTree did, and this keeps parsing what it output
 - working.
 -
 - (The --long format is not currently supported.) -}
parserLsTree :: A.Parser TreeItem
parserLsTree = TreeItem
	-- mode
	<$> octal
	<* A8.char ' '
	-- type
	<*> A8.takeTill (== ' ')
	<* A8.char ' '
	-- sha
	<*> (Ref <$> A8.takeTill A8.isSpace)
	<* A8.space
	-- file
	<*> (asTopFilePath . Git.Filename.decode <$> A.takeByteString)

{- Inverse of parseLsTree -}
formatLsTree :: TreeItem -> String
formatLsTree ti = unwords
	[ showOct (mode ti) ""
	, decodeBS (typeobj ti)
	, fromRef (sha ti)
	] ++ ('\t' : fromRawFilePath (getTopFilePath (file ti)))

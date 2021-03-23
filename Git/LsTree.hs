{- git ls-tree interface
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.LsTree (
	TreeItem(..),
	LsTreeRecursive(..),
	LsTreeLong(..),
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
	, size :: Maybe FileSize
	, file :: TopFilePath
	-- ^ only available when long is used
	} deriving (Show)

data LsTreeRecursive = LsTreeRecursive | LsTreeNonRecursive

{- Enabling --long also gets the size of tree items.
 - This slows down ls-tree some, since it has to look up the size of each
 - blob.
 -}
data LsTreeLong = LsTreeLong Bool

{- Lists the contents of a tree, with lazy output. -}
lsTree :: LsTreeRecursive -> LsTreeLong -> Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree = lsTree' []

lsTree' :: [CommandParam] -> LsTreeRecursive -> LsTreeLong -> Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree' ps recursive long t repo = do
	(l, cleanup) <- pipeNullSplit (lsTreeParams recursive long t ps) repo
	return (rights (map (parseLsTree long) l), cleanup)

lsTreeStrict :: LsTreeRecursive -> LsTreeLong -> Ref -> Repo -> IO [TreeItem]
lsTreeStrict = lsTreeStrict' []

lsTreeStrict' :: [CommandParam] -> LsTreeRecursive -> LsTreeLong -> Ref -> Repo -> IO [TreeItem]
lsTreeStrict' ps recursive long t repo = rights . map (parseLsTreeStrict long)
	<$> pipeNullSplitStrict (lsTreeParams recursive long t ps) repo

lsTreeParams :: LsTreeRecursive -> LsTreeLong -> Ref -> [CommandParam] -> [CommandParam]
lsTreeParams recursive long r ps =
	[ Param "ls-tree"
	, Param "--full-tree"
	, Param "-z"
	] ++ recursiveparams ++ longparams ++ ps ++
	[ Param "--"
	, File $ fromRef r
	]
  where
	recursiveparams = case recursive of
		LsTreeRecursive -> [ Param "-r" ]
		LsTreeNonRecursive -> []
	longparams = case long of
		LsTreeLong True -> [ Param "--long" ]
		LsTreeLong False -> []

{- Lists specified files in a tree. -}
lsTreeFiles :: LsTreeLong -> Ref -> [FilePath] -> Repo -> IO [TreeItem]
lsTreeFiles long t fs repo = rights . map (parseLsTree long . L.fromStrict)
	<$> pipeNullSplitStrict ps repo
  where
	ps =
		[ Param "ls-tree"
		, Param "--full-tree"
		, Param "-z"
		, Param "--"
		, File $ fromRef t
		] ++ map File fs

parseLsTree :: LsTreeLong -> L.ByteString -> Either String TreeItem
parseLsTree long b = case A.parse (parserLsTree long) b of
	A.Done _ r  -> Right r
	A.Fail _ _ err -> Left err

parseLsTreeStrict :: LsTreeLong -> S.ByteString -> Either String TreeItem
parseLsTreeStrict long b = go (AS.parse (parserLsTree long) b)
  where
	go (AS.Done _ r) = Right r
	go (AS.Fail _ _ err) = Left err
	go (AS.Partial c) = go (c mempty)

{- Parses a line of ls-tree output, in format:
 -   mode SP type SP sha TAB file
 - Or long format:
 -   mode SP type SP sha SPACES size TAB file
 -
 - The TAB can also be a space. Git does not use that, but an earlier
 - version of formatLsTree did, and this keeps parsing what it output
 - working.
 -}
parserLsTree :: LsTreeLong -> A.Parser TreeItem
parserLsTree long = case long of
	LsTreeLong False -> 
		startparser <*> pure Nothing <* filesep <*> fileparser
	LsTreeLong True ->
		startparser <* sizesep <*> sizeparser <* filesep <*> fileparser
  where
	startparser = TreeItem
		-- mode
		<$> octal
		<* A8.char ' '
		-- type
		<*> A8.takeTill (== ' ')
		<* A8.char ' '
		-- sha
		<*> (Ref <$> A8.takeTill A8.isSpace)

	fileparser = asTopFilePath . Git.Filename.decode <$> A.takeByteString

	sizeparser = fmap Just A8.decimal

	filesep = A8.space

	sizesep = A.many1 A8.space

{- Inverse of parseLsTree. Note that the long output format is not
 - generated, so any size information is not included. -}
formatLsTree :: TreeItem -> String
formatLsTree ti = unwords
	[ showOct (mode ti) ""
	, decodeBS (typeobj ti)
	, fromRef (sha ti)
	] ++ ('\t' : fromRawFilePath (getTopFilePath (file ti)))

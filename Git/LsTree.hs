{- git ls-tree interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.LsTree (
	lsTree
) where

import Numeric
import Control.Applicative
import Data.Char

import Git
import Utility.SafeCommand

type Treeish = String

data TreeItem = TreeItem
	{ mode :: Int
	, objtype :: String
	, sha :: String
	, file :: FilePath
	} deriving Show

{- Lists the contents of a Treeish -}
lsTree :: Repo -> Treeish -> IO [TreeItem]
lsTree repo t = map parseLsTree <$>
	pipeNullSplit repo [Params "ls-tree --full-tree -z -r --", File t]

{- Parses a line of ls-tree output.
 - (The --long format is not currently supported.) -}
parseLsTree :: String -> TreeItem
parseLsTree l = TreeItem m o s f
	where
		-- l = <mode> SP <type> SP <sha> TAB <file>
		-- Since everything until the file is fixed-width,
		-- do not need to split on words.
		(m, past_m) = head $ readOct l
		(o, past_o) = splitAt 4 $ space past_m
		(s, past_s) = splitAt shaSize $ space past_o
		f = decodeGitFile $ space past_s
		space s@(sp:rest)
			| isSpace sp = rest
			| otherwise = error $
				"ls-tree parse error at '" ++ s ++ "' in " ++ l

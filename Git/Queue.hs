{- git repository command queue
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Git.Queue (
	Queue,
	new,
	add,
	size,
	full,
	flush,
) where

import qualified Data.Map as M
import System.IO
import System.Cmd.Utils
import Data.String.Utils

import Utility.SafeCommand
import Common
import Git
import Git.Command

{- An action to perform in a git repository. The file to act on
 - is not included, and must be able to be appended after the params. -}
data Action = Action
	{ getSubcommand :: String
	, getParams :: [CommandParam]
	} deriving (Show, Eq, Ord)

{- A queue of actions to perform (in any order) on a git repository,
 - with lists of files to perform them on. This allows coalescing 
 - similar git commands. -}
data Queue = Queue
	{ size :: Int
	, _limit :: Int
	, _items :: M.Map Action [FilePath]
	}
	deriving (Show, Eq)

{- A recommended maximum size for the queue, after which it should be
 - run.
 -
 - 10240 is semi-arbitrary. If we assume git filenames are between 10 and
 - 255 characters long, then the queue will build up between 100kb and
 - 2550kb long commands. The max command line length on linux is somewhere
 - above 20k, so this is a fairly good balance -- the queue will buffer
 - only a few megabytes of stuff and a minimal number of commands will be
 - run by xargs. -}
defaultLimit :: Int
defaultLimit = 10240

{- Constructor for empty queue. -}
new :: Maybe Int -> Queue
new lim = Queue 0 (fromMaybe defaultLimit lim) M.empty

{- Adds an action to a queue. -}
add :: Queue -> String -> [CommandParam] -> [FilePath] -> Queue
add (Queue cur lim m) subcommand params files = Queue (cur + 1) lim m'
	where
		action = Action subcommand params
		-- There are probably few items in the map, but there
		-- can be a lot of files per item. So, optimise adding
		-- files.
		m' = M.insertWith' const action fs m
		!fs = files ++ M.findWithDefault [] action m

{- Is a queue large enough that it should be flushed? -}
full :: Queue -> Bool
full (Queue cur lim  _) = cur > lim

{- Runs a queue on a git repository. -}
flush :: Queue -> Repo -> IO Queue
flush (Queue _ lim m) repo = do
	forM_ (M.toList m) $ uncurry $ runAction repo
	return $ Queue 0 lim M.empty

{- Runs an Action on a list of files in a git repository.
 -
 - Complicated by commandline length limits.
 -
 - Intentionally runs the command even if the list of files is empty;
 - this allows queueing commands that do not need a list of files. -}
runAction :: Repo -> Action -> [FilePath] -> IO ()
runAction repo action files =
	pOpen WriteToPipe "xargs" ("-0":"git":params) feedxargs
	where
		params = toCommand $ gitCommandLine
			(Param (getSubcommand action):getParams action) repo
		feedxargs h = do
			fileEncoding h
			hPutStr h $ join "\0" files

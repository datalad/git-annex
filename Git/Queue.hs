{- git repository command queue
 -
 - Copyright 2010,2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Git.Queue (
	Queue,
	new,
	addCommand,
	addUpdateIndex,
	size,
	full,
	flush,
	merge,
) where

import Utility.SafeCommand
import Common
import Git
import Git.Command
import qualified Git.UpdateIndex

import qualified Data.Map as M

{- Queable actions that can be performed in a git repository. -}
data Action
	{- Updating the index file, using a list of streamers that can
	 - be added to as the queue grows. -}
	= UpdateIndexAction [Git.UpdateIndex.Streamer] -- in reverse order
	{- A git command to run, on a list of files that can be added to
	 - as the queue grows. -}
	| CommandAction 
		{ getSubcommand :: String
		, getParams :: [CommandParam]
		, getFiles :: [CommandParam]
		} 

{- A key that can uniquely represent an action in a Map. -}
data ActionKey = UpdateIndexActionKey | CommandActionKey String
	deriving (Eq, Ord)

actionKey :: Action -> ActionKey
actionKey (UpdateIndexAction _) = UpdateIndexActionKey
actionKey CommandAction { getSubcommand = s } = CommandActionKey s

{- A queue of actions to perform (in any order) on a git repository,
 - with lists of files to perform them on. This allows coalescing 
 - similar git commands. -}
data Queue = Queue
	{ size :: Int
	, _limit :: Int
	, items :: M.Map ActionKey Action
	}

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

{- Adds an git command to the queue.
 -
 - Git commands with the same subcommand but different parameters are
 - assumed to be equivilant enough to perform in any order with the same
 - result.
 -}
addCommand :: String -> [CommandParam] -> [FilePath] -> Queue -> Repo -> IO Queue
addCommand subcommand params files q repo =
	updateQueue action different (length files) q repo
  where
	action = CommandAction
		{ getSubcommand = subcommand
		, getParams = params
		, getFiles = map File files
		}
	
	different (CommandAction { getSubcommand = s }) = s /= subcommand
	different _ = True

{- Adds an update-index streamer to the queue. -}
addUpdateIndex :: Git.UpdateIndex.Streamer -> Queue -> Repo -> IO Queue
addUpdateIndex streamer q repo =
	updateQueue action different 1 q repo
  where
	-- the list is built in reverse order
	action = UpdateIndexAction [streamer]

	different (UpdateIndexAction _) = False
	different _ = True

{- Updates or adds an action in the queue. If the queue already contains a
 - different action, it will be flushed; this is to ensure that conflicting
 - actions, like add and rm, are run in the right order.-}
updateQueue :: Action -> (Action -> Bool) -> Int -> Queue -> Repo -> IO Queue
updateQueue !action different sizeincrease q repo
	| null (filter different (M.elems (items q))) = return $ go q
	| otherwise = go <$> flush q repo
  where
	go q' = newq
	  where		
		!newq = q'
			{ size = newsize
			, items = newitems
			}
		!newsize = size q' + sizeincrease
		!newitems = M.insertWith' combineNewOld (actionKey action) action (items q')

combineNewOld :: Action -> Action -> Action
combineNewOld (CommandAction _sc1 _ps1 fs1) (CommandAction sc2 ps2 fs2) =
	CommandAction sc2 ps2 (fs1++fs2)
combineNewOld (UpdateIndexAction s1) (UpdateIndexAction s2) =
	UpdateIndexAction (s1++s2)
combineNewOld anew _aold = anew

{- Merges the contents of the second queue into the first.
 - This should only be used when the two queues are known to contain
 - non-conflicting actions. -}
merge :: Queue -> Queue -> Queue
merge origq newq = origq
	{ size = size origq + size newq
	, items = M.unionWith combineNewOld (items newq) (items origq)
	}

{- Is a queue large enough that it should be flushed? -}
full :: Queue -> Bool
full (Queue cur lim  _) = cur >= lim

{- Runs a queue on a git repository. -}
flush :: Queue -> Repo -> IO Queue
flush (Queue _ lim m) repo = do
	forM_ (M.elems m) $ runAction repo
	return $ Queue 0 lim M.empty

{- Runs an Action on a list of files in a git repository.
 -
 - Complicated by commandline length limits.
 -
 - Intentionally runs the command even if the list of files is empty;
 - this allows queueing commands that do not need a list of files. -}
runAction :: Repo -> Action -> IO ()
runAction repo (UpdateIndexAction streamers) =
	-- list is stored in reverse order
	Git.UpdateIndex.streamUpdateIndex repo $ reverse streamers
runAction repo action@(CommandAction {}) = do
#ifndef mingw32_HOST_OS
	let p = (proc "xargs" $ "-0":"git":toCommand gitparams) { env = gitEnv repo }
	withHandle StdinHandle createProcessSuccess p $ \h -> do
		fileEncoding h
		hPutStr h $ intercalate "\0" $ toCommand $ getFiles action
		hClose h
#else
	-- Using xargs on Windows is problimatic, so just run the command
	-- once per file (not as efficient.)
	if null (getFiles action)
		then void $ boolSystemEnv "git" gitparams (gitEnv repo)
		else forM_ (getFiles action) $ \f ->
			void $ boolSystemEnv "git" (gitparams ++ [f]) (gitEnv repo)
#endif
  where
	gitparams = gitCommandLine
		(Param (getSubcommand action):getParams action) repo

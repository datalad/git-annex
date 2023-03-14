{- git repository command queue
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Git.Queue (
	Queue,
	new,
	defaultTimelimit,
	addCommand,
	addUpdateIndex,
	addFlushAction,
	FlushActionRunner(..),
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

import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Clock.POSIX

{- Queable actions that can be performed in a git repository. -}
data Action m
	{- Updating the index file, using a list of streamers that can
	 - be added to as the queue grows. -}
	= UpdateIndexAction [Git.UpdateIndex.Streamer] -- in reverse order
	{- A git command to run, on a list of files that can be added to
	 - as the queue grows. -}
	| CommandAction 
		{ getCommonParams :: [CommandParam]
		-- ^ parameters that come before the git subcommand
		-- (in addition to the Repo's gitGlobalOpts.
		, getSubcommand :: String
		, getParams :: [CommandParam]
		-- ^ parameters that come after the git subcommand
		, getFiles :: [CommandParam]
		} 
	{- A FlushAction can be added along with CommandActions or
	 - UpdateIndexActions, and when the queue later gets flushed,
	 - those will be run before the FlushAction is. -}
	| FlushAction
		{ getFlushActionRunner :: FlushActionRunner m
		, getFlushActionFiles :: [RawFilePath]
		}

{- The String must be unique for each flush action. -}
data FlushActionRunner m = FlushActionRunner String (Repo -> [RawFilePath] -> m ())

instance Eq (FlushActionRunner m) where
	FlushActionRunner s1 _ == FlushActionRunner s2 _ = s1 == s2

{- A key that can uniquely represent an action in a Map.
 -
 - The ordering controls what order the actions are run in when flushing
 - the queue. -}
data ActionKey
	= UpdateIndexActionKey
	| CommandActionKey [CommandParam] String [CommandParam]
	| FlushActionKey String
	deriving (Eq, Ord)

actionKey :: Action m -> ActionKey
actionKey (UpdateIndexAction _) = UpdateIndexActionKey
actionKey CommandAction { getCommonParams = c, getSubcommand = s, getParams = p } = CommandActionKey c s p
actionKey FlushAction { getFlushActionRunner = FlushActionRunner s _ } = FlushActionKey s

{- A queue of actions to perform (in any order) on a git repository,
 - with lists of files to perform them on. This allows coalescing 
 - similar git commands. -}
data Queue m = Queue
	{ size :: Int
	, _limit :: Int
	, _timelimit :: NominalDiffTime
	, _lastchanged :: POSIXTime
	, items :: M.Map ActionKey (Action m)
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

{- How close together in seconds changes to the queue have to be happening
 - in order for it to keep accumulating actions, rather than running actions
 - immediately. -}
defaultTimelimit :: NominalDiffTime
defaultTimelimit = 60 * 5

{- Constructor for empty queue. -}
new :: Maybe Int -> Maybe NominalDiffTime -> IO (Queue m)
new lim tlim = do
	now <- getPOSIXTime
	return $ Queue 0
		(fromMaybe defaultLimit lim)
		(fromMaybe defaultTimelimit tlim)
		now
		M.empty

{- Adds an git command to the queue.
 -
 - Git commands with the same subcommand but different parameters are
 - assumed to be equivalent enough to perform in any order with the same
 - end result.
 -}
addCommand :: MonadIO m => [CommandParam] -> String -> [CommandParam] -> [FilePath] -> Queue m -> Repo -> m (Queue m)
addCommand commonparams subcommand params files q repo =
	updateQueue action conflicting (length files) q repo
  where
	action = CommandAction
		{ getCommonParams = commonparams
		, getSubcommand = subcommand
		, getParams = params
		, getFiles = map File files
		}
	
	conflicting (CommandAction { getSubcommand = s }) = s /= subcommand
	conflicting (FlushAction {}) = False
	conflicting _ = True

{- Adds an flush action to the queue. This can co-exist with anything else
 - that gets added to the queue, and when the queue is eventually flushed,
 - it will be run after the other things in the queue. -}
addFlushAction :: MonadIO m => FlushActionRunner m -> [RawFilePath] -> Queue m -> Repo -> m (Queue m)
addFlushAction runner files q repo =
	updateQueue action (const False) (length files) q repo
  where
	action = FlushAction
		{ getFlushActionRunner = runner
		, getFlushActionFiles = files
		}

{- Adds an update-index streamer to the queue. -}
addUpdateIndex :: MonadIO m => Git.UpdateIndex.Streamer -> Queue m -> Repo -> m (Queue m)
addUpdateIndex streamer q repo =
	updateQueue action conflicting 1 q repo
  where
	-- the list is built in reverse order
	action = UpdateIndexAction [streamer]

	conflicting (UpdateIndexAction _) = False
	conflicting (FlushAction {}) = False
	conflicting _ = True

{- Updates or adds an action in the queue.
 -
 - If the queue already contains a conflicting action, it will be flushed
 - before adding the action; this is to ensure that conflicting actions,
 - like add and rm, are run in the right order.
 -
 - If the queue's time limit has been exceeded, it will also be flushed,
 - and the action will be run right away.
 -}
updateQueue :: MonadIO m => Action m -> (Action m -> Bool) -> Int -> Queue m -> Repo -> m (Queue m)
updateQueue !action conflicting sizeincrease q repo = do
	now <- liftIO getPOSIXTime
	if now - (_lastchanged q) > _timelimit q
		then if isconflicting
			then do
				q' <- flush q repo
				flush (mk q') repo
			else flush (mk q) repo
		else if isconflicting
			then mk <$> flush q repo
			else return $ mk (q { _lastchanged = now })
  where
	isconflicting = not (null (filter conflicting (M.elems (items q))))
	mk q' = newq
	  where		
		!newq = q'
			{ size = newsize
			, items = newitems
			}
		!newsize = size q' + sizeincrease
		!newitems = M.insertWith combineNewOld (actionKey action) action (items q')

{- The new value comes first. It probably has a smaller list of files than
 - the old value. So, the list append of the new value first is more
 - efficient. -}
combineNewOld :: Action m -> Action m -> Action m
combineNewOld (CommandAction _cps1 _sc1 _ps1 fs1) (CommandAction cps2 sc2 ps2 fs2) =
	CommandAction cps2 sc2 ps2 (fs1++fs2)
combineNewOld (UpdateIndexAction s1) (UpdateIndexAction s2) =
	UpdateIndexAction (s1++s2)
combineNewOld (FlushAction _r1 fs1) (FlushAction r2 fs2) =
	FlushAction r2 (fs1++fs2)
combineNewOld anew _aold = anew

{- Merges the contents of the second queue into the first.
 - This should only be used when the two queues are known to contain
 - non-conflicting actions. -}
merge :: Queue m -> Queue m -> Queue m
merge origq newq = origq
	{ size = size origq + size newq
	, items = M.unionWith combineNewOld (items newq) (items origq)
	, _lastchanged = max (_lastchanged origq) (_lastchanged newq)
	}

{- Is a queue large enough that it should be flushed? -}
full :: Queue m -> Bool
full (Queue cur lim _ _ _) = cur >= lim

{- Runs a queue on a git repository. -}
flush :: MonadIO m => Queue m -> Repo -> m (Queue m)
flush (Queue _ lim tlim _ m) repo = do
	forM_ (M.elems m) $ runAction repo
	now <- liftIO getPOSIXTime
	return $ Queue 0 lim tlim now M.empty

{- Runs an Action on a list of files in a git repository.
 -
 - Complicated by commandline length limits.
 -
 - Intentionally runs the command even if the list of files is empty;
 - this allows queueing commands that do not need a list of files. -}
runAction :: MonadIO m => Repo -> Action m -> m ()
runAction repo (UpdateIndexAction streamers) =
	-- list is stored in reverse order
	liftIO $ Git.UpdateIndex.streamUpdateIndex repo $ reverse streamers
runAction repo action@(CommandAction {}) = liftIO $ do
#ifndef mingw32_HOST_OS
	let p = (proc "xargs" $ "-0":"git":toCommand gitparams)
		{ env = gitEnv repo
		, std_in = CreatePipe
		}
	withCreateProcess p (go p)
#else
	-- Using xargs on Windows is problematic, so just run the command
	-- once per file (not as efficient.)
	if null (getFiles action)
		then void $ boolSystemEnv "git" gitparams (gitEnv repo)
		else forM_ (getFiles action) $ \f ->
			void $ boolSystemEnv "git" (gitparams ++ [f]) (gitEnv repo)
#endif
  where
	gitparams = gitCommandLine
		(getCommonParams action++Param (getSubcommand action):getParams action) 
		repo
#ifndef mingw32_HOST_OS
	go p (Just h) _ _ pid = do
		hPutStr h $ intercalate "\0" $ toCommand $ getFiles action
		hClose h
		forceSuccessProcess p pid
	go _ _ _ _ _ = error "internal"
#endif
runAction repo action@(FlushAction {}) =
	let FlushActionRunner _ runner = getFlushActionRunner action
	in runner repo (getFlushActionFiles action)

{- git-annex actions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Action (
	startup,
	shutdown,
	stopCoProcesses,
	reapZombies,
) where

import qualified Data.Map as M
#ifndef mingw32_HOST_OS
import System.Posix.Process (getAnyProcessStatus)
import Utility.Exception
#endif

import Annex.Common
import qualified Annex
import Annex.Content
import Annex.CatFile
import Annex.CheckAttr
import Annex.HashObject
import Annex.CheckIgnore

{- Actions to perform each time ran. -}
startup :: Annex ()
startup = return ()

{- Cleanup actions. -}
shutdown :: Bool -> Annex ()
shutdown nocommit = do
	saveState nocommit
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanup
	stopCoProcesses
	liftIO reapZombies -- zombies from long-running git processes

{- Stops all long-running git query processes. -}
stopCoProcesses :: Annex ()
stopCoProcesses = do
	catFileStop
	checkAttrStop
	hashObjectStop
	checkIgnoreStop

{- Reaps any zombie processes that may be hanging around.
 -
 - Warning: Not thread safe. Anything that was expecting to wait
 - on a process and get back an exit status is going to be confused
 - if this reap gets there first. -}
reapZombies :: IO ()
#ifndef mingw32_HOST_OS
reapZombies =
	-- throws an exception when there are no child processes
	catchDefaultIO Nothing (getAnyProcessStatus False True)
		>>= maybe (return ()) (const reapZombies)

#else
reapZombies = return ()
#endif

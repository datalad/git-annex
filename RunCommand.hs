{- git-annex running commands
 -
 - Copyright 2010-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module RunCommand where

import Common.Annex
import qualified Annex
import Types.Command
import qualified Annex.Queue
import Annex.Exception

{- Runs a command, starting with the check stage, and then
 - the seek stage. Finishes by printing the number of commandActions that
 - failed. -}
performCommand :: Command -> CmdParams -> Annex ()
performCommand Command { cmdseek = seek, cmdcheck = c, cmdname = name } params = do
	mapM_ runCheck c
	Annex.changeState $ \s -> s { Annex.errcounter = 0 }
	seek params
	showerrcount =<< Annex.getState Annex.errcounter
  where
	showerrcount 0 = noop
	showerrcount cnt = error $ name ++ ": " ++ show cnt ++ " failed"

{- Runs one of the actions needed to perform a command.
 - Individual actions can fail without stopping the whole command,
 - including by throwing IO errors (but other errors terminate the whole
 - command).
 - 
 - This should only be run in the seek stage. -}
commandAction :: CommandStart -> Annex ()
commandAction a = handle =<< tryAnnexIO go
  where
	go = do
		Annex.Queue.flushWhenFull
		callCommand a
	handle (Right True) = noop
	handle (Right False) = incerr
	handle (Left err) = do
		showErr err
		showEndFail
		incerr
	incerr = Annex.changeState $ \s -> 
		let ! c = Annex.errcounter s + 1 
		    ! s' = s { Annex.errcounter = c }
		in s'

{- Runs a single command action through the start, perform and cleanup stages -}
callCommand :: CommandStart -> CommandCleanup
callCommand = start
  where
	start   = stage $ maybe skip perform
	perform = stage $ maybe failure cleanup
	cleanup = stage $ status
	stage = (=<<)
	skip = return True
	failure = showEndFail >> return False
	status r = showEndResult r >> return r

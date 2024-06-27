{- git-annex actions
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Action (
	action,
	verifiedAction,
	quiesce,
	stopCoProcesses,
) where

import qualified Data.Map as M

import Annex.Common
import qualified Annex
import Annex.Content
import Annex.CatFile
import Annex.CheckAttr
import Annex.HashObject
import Annex.CheckIgnore
import Annex.TransferrerPool
import qualified Database.Keys

{- Runs an action that may throw exceptions, catching and displaying them. -}
action :: Annex () -> Annex Bool
action a = tryNonAsync a >>= \case
	Right () -> return True
	Left e -> do
		warning (UnquotedString (show e))
		return False

verifiedAction :: Annex Verification -> Annex (Bool, Verification)
verifiedAction a = tryNonAsync a >>= \case
	Right v -> return (True, v)
	Left e -> do
		warning (UnquotedString (show e))
		return (False, UnVerified)

{- Rn all cleanup actions, save all state, stop all long-running child
 - processes.
 -
 - This can be run repeatedly with other Annex actions run in between,
 - but usually it is run only once at the end.
 -
 - When passed True, avoids making any commits to the git-annex branch,
 - leaving changes in the journal for later commit.
 -}
quiesce :: Bool -> Annex ()
quiesce nocommit = do
	cas <- Annex.withState $ \st -> return 
		( st { Annex.cleanupactions = mempty }
		, Annex.cleanupactions st
		)
	sequence_ (M.elems cas)
	saveState nocommit
	stopCoProcesses
	Database.Keys.closeDb

{- Stops all long-running child processes, including git query processes. -}
stopCoProcesses :: Annex ()
stopCoProcesses = do
	catFileStop
	checkAttrStop
	hashObjectStop
	checkIgnoreStop
	emptyTransferrerPool

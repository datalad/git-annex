{- git-annex actions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Action (
	startup,
	shutdown,
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

{- Actions to perform each time ran. -}
startup :: Annex ()
startup = return ()

{- Cleanup actions. -}
shutdown :: Bool -> Annex ()
shutdown nocommit = do
	saveState nocommit
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanup
	stopCoProcesses

{- Stops all long-running git query processes. -}
stopCoProcesses :: Annex ()
stopCoProcesses = do
	catFileStop
	checkAttrStop
	hashObjectStop
	checkIgnoreStop

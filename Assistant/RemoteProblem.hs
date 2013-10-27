{- git-annex assistant remote problem handling
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.RemoteProblem where

import Assistant.Common
import Utility.TList

import Control.Concurrent.STM

{- Gets all remotes that have problems.
 - Blocks until there is at least one. -}
getRemoteProblems :: Assistant [Remote]
getRemoteProblems = (atomically . getTList) <<~ remoteProblemChan

{- Indicates that there was a problem accessing a remote, and the problem
 - appears to not be a transient (eg network connection) problem. -}
remoteHasProblem :: Remote -> Assistant ()
remoteHasProblem r  = (atomically . flip consTList r) <<~ remoteProblemChan

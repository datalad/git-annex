{- git-annex assistant remote problem handling
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.RepoProblem where

import Assistant.Common
import Utility.TList

import Control.Concurrent.STM

{- Gets all repositories that have problems. 
 - Blocks until there is at least one. -}
getRepoProblems :: Assistant [UUID]
getRepoProblems = (atomically . getTList) <<~ repoProblemChan

{- Indicates that there was a problem accessing a repo, and the problem
 - appears to not be a transient (eg network connection) problem. -}
repoHasProblem :: UUID -> Assistant ()
repoHasProblem r  = (atomically . flip consTList r) <<~ repoProblemChan

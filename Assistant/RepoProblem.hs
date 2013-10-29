{- git-annex assistant remote problem handling
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.RepoProblem where

import Assistant.Common
import Assistant.Types.RepoProblem
import Utility.TList

import Control.Concurrent.STM

{- Gets all repositories that have problems. Blocks until there is at
 - least one. -}
getRepoProblems :: Assistant [RepoProblem]
getRepoProblems = nubBy sameRepoProblem
	<$> (atomically . getTList) <<~ repoProblemChan

{- Indicates that there was a problem with a repository, and the problem
 - appears to not be a transient (eg network connection) problem.
 -
 - If the problem is able to be repaired, the passed action will be run.
 - (However, if multiple problems are reported with a single repository,
 - only a single action will be run.)
 -}
repoHasProblem :: UUID -> Assistant () -> Assistant ()
repoHasProblem u afterrepair = do
	rp <- RepoProblem
		<$> pure u
		<*> asIO afterrepair
	(atomically . flip consTList rp) <<~ repoProblemChan

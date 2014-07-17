{- Utilities for git remotes.
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Git where

import Common.Annex
import qualified Git
import Types.Availability

repoCheap :: Git.Repo -> Bool
repoCheap = not . Git.repoIsUrl

localpathCalc :: Git.Repo -> Maybe FilePath
localpathCalc r
	| availabilityCalc r == GloballyAvailable = Nothing
	| otherwise = Just $ Git.repoPath r

availabilityCalc :: Git.Repo -> Availability
availabilityCalc r
	| (Git.repoIsLocal r || Git.repoIsLocalUnknown r) = LocallyAvailable
	| otherwise = GloballyAvailable

{- Avoids performing an action on a local repository that's not usable.
 - Does not check that the repository is still available on disk. -}
guardUsable :: Git.Repo -> a -> Annex a -> Annex a
guardUsable r onerr a
	| Git.repoIsLocalUnknown r = return onerr
	| otherwise = a

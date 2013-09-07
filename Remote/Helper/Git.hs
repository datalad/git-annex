{- Utilities for git remotes.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Git where

import Common.Annex
import qualified Git

repoCheap :: Git.Repo -> Bool
repoCheap = not . Git.repoIsUrl

localpathCalc :: Git.Repo -> Maybe FilePath
localpathCalc r = if globallyAvailableCalc r
	then Nothing
	else Just $ Git.repoPath r

globallyAvailableCalc :: Git.Repo -> Bool
globallyAvailableCalc r = not $
	Git.repoIsLocal r || Git.repoIsLocalUnknown r

{- Avoids performing an action on a local repository that's not usable.
 - Does not check that the repository is still available on disk. -}
guardUsable :: Git.Repo -> a -> Annex a -> Annex a
guardUsable r onerr a
	| Git.repoIsLocalUnknown r = return onerr
	| otherwise = a

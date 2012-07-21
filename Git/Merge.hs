{- git merging
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Merge where

import Common
import Git
import Git.Command

{- Avoids recent git's interactive merge. -}
mergeNonInteractive :: Ref -> Repo -> IO Bool
mergeNonInteractive branch = runBool "merge"
	[Param "--no-edit", Param $ show branch]

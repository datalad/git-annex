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
import Git.Version

{- Avoids recent git's interactive merge. -}
mergeNonInteractive :: Ref -> Repo -> IO Bool
mergeNonInteractive branch
	| older "1.7.7.6" = runBool "merge" [Param $ show branch]
	| otherwise = runBool "merge" [Param "--no-edit", Param $ show branch]

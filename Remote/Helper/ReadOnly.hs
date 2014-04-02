{- Adds readonly support to remotes.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.ReadOnly (adjustReadOnly) where

import Common.Annex
import Types.Remote

{- Adds support for read-only remotes, by replacing the
 - methods that write to a remote with dummies that fail.
 -
 - Note that disabling git pushes to remotes is not handled here.
 -}
adjustReadOnly :: Remote -> Remote
adjustReadOnly r
	| remoteAnnexReadOnly (gitconfig r) = r
		{ storeKey = \_ _ _ -> failbool
		, removeKey = \_ -> failbool
		, repairRepo = Nothing
		}
	| otherwise = r
  where
	failbool = do
		warning "this remote is readonly"
		return False

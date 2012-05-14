{- git core.sharedRepository handling
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.SharedRepository where

import Data.Char

import Common
import Git
import qualified Git.Config

data SharedRepository = UnShared | GroupShared | AllShared | UmaskShared Int

getSharedRepository :: Repo -> SharedRepository
getSharedRepository r =
	case map toLower $ Git.Config.get "core.sharedrepository" "" r of
		"1" -> GroupShared
		"group" -> GroupShared
		"true" -> GroupShared
		"all" -> AllShared
		"world" -> AllShared
		"everybody" -> AllShared
		v -> maybe UnShared UmaskShared (readish v)

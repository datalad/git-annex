{- git config types
 -
 - Copyright 2012, 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.ConfigTypes where

import Data.Char

import Common
import Git
import qualified Git.Config

data SharedRepository = UnShared | GroupShared | AllShared | UmaskShared Int
	deriving (Eq)

getSharedRepository :: Repo -> SharedRepository
getSharedRepository r =
	case map toLower $ Git.Config.get "core.sharedrepository" "" r of
		"1" -> GroupShared
		"2" -> AllShared
		"group" -> GroupShared
		"true" -> GroupShared
		"all" -> AllShared
		"world" -> AllShared
		"everybody" -> AllShared
		v -> maybe UnShared UmaskShared (readish v)

data DenyCurrentBranch = UpdateInstead | RefusePush | WarnPush | IgnorePush
	deriving (Eq)

getDenyCurrentBranch :: Repo -> DenyCurrentBranch
getDenyCurrentBranch r =
	case map toLower $ Git.Config.get "receive.denycurrentbranch" "" r of
		"updateinstead" -> UpdateInstead
		"warn" -> WarnPush
		"ignore" -> IgnorePush
		_ -> RefusePush

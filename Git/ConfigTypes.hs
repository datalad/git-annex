{- git config types
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.ConfigTypes where

import Data.Char
import Numeric
import qualified Data.ByteString.Char8 as S8

import Common
import Git
import Git.Types
import qualified Git.Config

data SharedRepository = UnShared | GroupShared | AllShared | UmaskShared Int
	deriving (Eq)

getSharedRepository :: Repo -> SharedRepository
getSharedRepository r =
	case Git.Config.getMaybe "core.sharedrepository" r of
		Just (ConfigValue v) -> case S8.map toLower v of
			"1" -> GroupShared
			"2" -> AllShared
			"group" -> GroupShared
			"true" -> GroupShared
			"all" -> AllShared
			"world" -> AllShared
			"everybody" -> AllShared
			_ -> maybe UnShared UmaskShared 
				(fmap fst $ headMaybe $ readOct $ decodeBS v)
		Just NoConfigValue -> UnShared
		Nothing -> UnShared

data DenyCurrentBranch = UpdateInstead | RefusePush | WarnPush | IgnorePush
	deriving (Eq)

getDenyCurrentBranch :: Repo -> DenyCurrentBranch
getDenyCurrentBranch r = 
	case Git.Config.getMaybe "receive.denycurrentbranch" r of
		Just (ConfigValue v) -> case S8.map toLower v of
			"updateinstead" -> UpdateInstead
			"warn" -> WarnPush
			"ignore" -> IgnorePush
			_ -> RefusePush
		Just NoConfigValue -> RefusePush
		Nothing -> RefusePush

{- git-annex transitions data types
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Transitions where

import Utility.OsPath

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

data FileTransition
	= ChangeFile Builder
	| PreserveFile

type TransitionCalculator = OsPath -> L.ByteString -> FileTransition

{- portability shim for System.MountPoints
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Mounts (getMounts, Mntent(..)) where

import qualified System.MountPoints
import System.MountPoints (Mntent(..))

import Utility.Exception

getMounts :: IO [Mntent] 
getMounts = System.MountPoints.getMounts
	-- That will crash when the linux build is running on Android,
	-- so fall back to this.
	`catchNonAsync` const System.MountPoints.getProcMounts

{- portability shim for System.MountPoints
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Mounts (getMounts, Mntent(..)) where

import qualified System.MountPoints
import System.MountPoints (Mntent(..))

getMounts :: IO [Mntent] 
#ifndef __ANDROID__
getMounts = System.MountPoints.getMounts
#else
getMounts = System.MountPoints.getProcMounts
#endif

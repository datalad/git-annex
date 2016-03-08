{- disk free space checking shim
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

module Utility.DiskFree (
	getDiskFree,
	getDiskSize
) where

#ifndef __ANDROID__

import System.DiskSpace
import Utility.Applicative
import Utility.Exception

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree = catchMaybeIO . getAvailSpace

getDiskSize :: FilePath -> IO (Maybe Integer)
getDiskSize = fmap diskTotal <$$> catchMaybeIO . getDiskUsage

#else

#warning Building without disk free space checking support

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree _ = return Nothing

getDiskSize :: FilePath -> IO (Maybe Integer)
getDiskSize _ = return Nothing

#endif

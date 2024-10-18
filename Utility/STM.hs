{- support for old versions of the stm package
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.STM (
	module Control.Concurrent.STM,
#if ! MIN_VERSION_stm(2,5,1)
	writeTMVar
#endif
) where

import Control.Concurrent.STM

#if ! MIN_VERSION_stm(2,5,1)
writeTMVar :: TMVar t -> t -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new
#endif

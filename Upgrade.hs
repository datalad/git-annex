{- git-annex upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Upgrade where

import Common.Annex
import Annex.Version
#ifndef mingw32_HOST_OS
import qualified Upgrade.V0
import qualified Upgrade.V1
#endif
import qualified Upgrade.V2

upgrade :: Annex Bool
upgrade = go =<< getVersion
  where
#ifndef mingw32_HOST_OS
	go (Just "0") = Upgrade.V0.upgrade
	go (Just "1") = Upgrade.V1.upgrade
#else
	go (Just "0") = error "upgrade from v0 on Windows not supported"
	go (Just "1") = error "upgrade from v1 on Windows not supported"
#endif
	go (Just "2") = Upgrade.V2.upgrade
	go _ = return True

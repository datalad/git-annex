{- System.Directory without its conflicting isSymbolicLink
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

-- Disable warnings because only some versions of System.Directory export
-- isSymbolicLink.
{-# OPTIONS_GHC -fno-warn-tabs -w #-}

module Utility.SystemDirectory (
	module System.Directory
) where

import System.Directory hiding (isSymbolicLink)

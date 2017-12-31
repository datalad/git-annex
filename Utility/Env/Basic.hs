{- portable environment variables, without any dependencies
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Env.Basic where

import Utility.Exception
import Control.Applicative
import Data.Maybe
import Prelude
import qualified System.Environment as E

getEnv :: String -> IO (Maybe String)
getEnv = catchMaybeIO . E.getEnv

getEnvDefault :: String -> String -> IO String
getEnvDefault var fallback = fromMaybe fallback <$> getEnv var

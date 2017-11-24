{- Build man pages, for use by Makefile
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Build.Mans

main :: IO ()
main = buildMansOrWarn

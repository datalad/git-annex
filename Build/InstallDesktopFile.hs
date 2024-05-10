{- Generating and installing a desktop menu entry file and icon,
 - and a desktop autostart file. (And OSX equivalents.)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Main where

import Build.DesktopFile

import System.Environment

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify git-annex command"
	go (command:_) = install command

{- Determining if output is to a terminal.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Terminal (
	IsTerminal(..),
	checkIsTerminal,
) where

import System.IO
#ifdef mingw32_HOST_OS
import System.Win32.MinTTY (isMinTTYHandle)
import System.Win32.Types
import Graphics.Win32.Misc
import Control.Exception
#endif

newtype IsTerminal = IsTerminal Bool

checkIsTerminal :: Handle -> IO IsTerminal
checkIsTerminal h = do
#ifndef mingw32_HOST_OS
	b <- hIsTerminalDevice h
	return (IsTerminal b)
#else
	b <- hIsTerminalDevice h
	if b
		then return (IsTerminal b)
		else do
			h' <- getStdHandle sTD_OUTPUT_HANDLE
				`catch` \(_ :: IOError) ->
					return nullHANDLE
			if h' == nullHANDLE
				then return (IsTerminal False)
				else do
					b' <- isMinTTYHandle h'
					return (IsTerminal b')
#endif

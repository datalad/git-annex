{- path manipulation
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Path.Max where

import System.FilePath

#ifndef mingw32_HOST_OS
import Utility.Exception
import System.Posix.Files
import Data.List
import Control.Applicative
import Prelude
#endif

{- Maximum size to use for a file in a specified directory.
 -
 - Many systems have a 255 byte limit to the name of a file, 
 - so that's taken as the max if the system has a larger limit, or has no
 - limit.
 -}
fileNameLengthLimit :: FilePath -> IO Int
#ifdef mingw32_HOST_OS
fileNameLengthLimit _ = return 255
#else
fileNameLengthLimit dir = do
	-- getPathVar can fail due to statfs(2) overflow
	l <- catchDefaultIO 0 $
		fromIntegral <$> getPathVar dir FileNameLimit
	if l <= 0
		then return 255
		else return $ minimum [l, 255]
#endif

{- Functions that operate on OsPath, but treat the contents of files as
 - Strings.
 -
 - These functions all set the close-on-exec flag to True, unlike
 - the Prelude versions.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

module Utility.FileIO.String
(
#ifdef WITH_OSPATH
	readFileString,
	writeFileString,
	appendFileString,
#endif
) where

#ifdef WITH_OSPATH
import qualified Utility.FileIO.CloseOnExec as I
import Utility.OsPath (OsPath)
import Prelude (String, IO, (>>=))
import System.IO (IOMode(..), hGetContents, hPutStr)

readFileString :: OsPath -> IO String
readFileString f = I.openFile f ReadMode >>= hGetContents

writeFileString :: OsPath -> String -> IO ()
writeFileString f txt = I.withFile f WriteMode (\hdl -> hPutStr hdl txt)

appendFileString :: OsPath -> String -> IO ()
appendFileString f txt = I.withFile f AppendMode (\hdl -> hPutStr hdl txt)
#endif

{- moving files
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.MoveFile (
	moveFile,
) where

import Control.Monad
import System.IO.Error
import Prelude

#ifndef mingw32_HOST_OS
import System.PosixCompat.Files (isDirectory)
import Control.Monad.IfElse
import Utility.SafeCommand
#endif

import Utility.SystemDirectory
import Utility.Tmp
import Utility.Exception
import Utility.Monad
import Utility.FileSystemEncoding
import qualified Utility.RawFilePath as R

{- Moves one filename to another.
 - First tries a rename, but falls back to moving across devices if needed. -}
moveFile :: RawFilePath -> RawFilePath -> IO ()
moveFile src dest = tryIO (R.rename src dest) >>= onrename
  where
	onrename (Right _) = noop
	onrename (Left e)
		| isPermissionError e = rethrow
		| isDoesNotExistError e = rethrow
		| otherwise = viaTmp mv (fromRawFilePath dest) ()
	  where
		rethrow = throwM e

		mv tmp () = do
			-- copyFile is likely not as optimised as
			-- the mv command, so we'll use the command.
			--
			-- But, while Windows has a "mv", it does not
			-- seem very reliable, so use copyFile there.
#ifndef mingw32_HOST_OS	
			-- If dest is a directory, mv would move the file
			-- into it, which is not desired.
			whenM (isdir dest) rethrow
			ok <- boolSystem "mv"
				[ Param "-f"
				, Param (fromRawFilePath src)
				, Param tmp
				]
			let e' = e
#else
			r <- tryIO $ copyFile (fromRawFilePath src) tmp
			let (ok, e') = case r of
				Left err -> (False, err)
				Right _ -> (True, e)
#endif
			unless ok $ do
				-- delete any partial
				_ <- tryIO $ removeFile tmp
				throwM e'

#ifndef mingw32_HOST_OS	
	isdir f = do
		r <- tryIO $ R.getSymbolicLinkStatus f
		case r of
			(Left _) -> return False
			(Right s) -> return $ isDirectory s
#endif

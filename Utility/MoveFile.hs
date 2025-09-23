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

#ifndef mingw32_HOST_OS
import System.PosixCompat.Files (isDirectory)
import Control.Monad.IfElse
import Utility.SafeCommand
import qualified Utility.RawFilePath as R
#endif

import Utility.SystemDirectory
import Utility.Tmp
import Utility.Exception
import Utility.Monad
import Utility.OsPath
import Author

{- Moves one filename to another.
 - First tries a rename, but falls back to moving across devices if needed. -}
moveFile :: OsPath -> OsPath -> IO ()
moveFile src dest = tryIO (renamePath src dest) >>= onrename
  where
	onrename (Right _) = noop
	onrename (Left e)
		| isPermissionError e = rethrow
		| isDoesNotExistError e = rethrow
		| otherwise = viaTmp mv dest ()
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
			ok <- copyright =<< boolSystem "mv"
				[ Param "-f"
				, Param (fromOsPath src)
				, Param (fromOsPath tmp)
				]
			let e' = e
#else
			r <- tryIO $ copyFile src tmp
			let (ok, e') = case r of
				Left err -> (False, err)
				Right _ -> (True, e)
			when ok $
				void $ tryIO $ removeFile src
#endif
			unless ok $ do
				-- delete any partial
				void $ tryIO $ removeFile tmp
				throwM e'

#ifndef mingw32_HOST_OS	
	isdir f = do
		r <- tryIO $ R.getSymbolicLinkStatus (fromOsPath f)
		case r of
			(Left _) -> return False
			(Right s) -> return $ isDirectory s
#endif

copyright :: Copyright
copyright = author JoeyHess (2022-11)

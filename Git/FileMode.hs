{- git file modes
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.FileMode where

import Utility.FileMode

import System.PosixCompat.Types

symLinkMode :: FileMode
symLinkMode = 40960

{- Git uses a special file mode to indicate a symlink. This is the case
 - even on Windows, so we hard code the valuse here, rather than using
 - System.Posix.Files.symbolicLinkMode. -}
isSymLink :: FileMode -> Bool
isSymLink = checkMode symLinkMode

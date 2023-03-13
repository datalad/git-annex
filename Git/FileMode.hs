{- git file modes
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.FileMode where

import Utility.FileMode

symLinkMode :: FileMode
symLinkMode = 40960

{- Git uses a special file mode to indicate a symlink. This is the case
 - even on Windows, so we hard code the values here, rather than using
 - System.Posix.Files.symbolicLinkMode. -}
isSymLink :: FileMode -> Bool
isSymLink = checkMode symLinkMode

{- git remote stuff
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Remote.Remove where

import Common
import Git
import Git.Types
import qualified Git.Command
import qualified Git.Version

remove :: RemoteName -> Repo -> IO ()
remove remotename r = do
	old <- Git.Version.older "1.8.0"
	Git.Command.run
		[ Param "remote"
		-- name of this subcommand changed
		, Param $
			if old
				then "rm"
				else "remove"
		, Param remotename
		] r

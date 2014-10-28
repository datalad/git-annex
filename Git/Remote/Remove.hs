{- git remote stuff
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Remote.Remove where

import Common
import Git
import Git.Types
import qualified Git.Command
import qualified Git.BuildVersion

remove :: RemoteName -> Repo -> IO ()
remove remotename = Git.Command.run
	[ Param "remote"
	-- name of this subcommand changed
	, Param $
		if Git.BuildVersion.older "1.8.0"
			then "rm"
			else "remove"
	, Param remotename
	]

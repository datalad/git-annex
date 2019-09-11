{- git remote removal
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Remote.Remove where

import Common
import Git
import Git.Types
import qualified Git.Command

remove :: RemoteName -> Repo -> IO ()
remove remotename = Git.Command.run
	[ Param "remote"
	, Param "remove"
	, Param remotename
	]

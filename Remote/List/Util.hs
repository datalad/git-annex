{- git-annex remote list utils
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.List.Util where

import Annex.Common
import qualified Annex
import qualified Git.Config

{- Call when remotes have changed. Re-reads the git config, and
 - invalidates the cache so the remoteList will be re-generated next time
 - it's used. -}
remotesChanged :: Annex ()
remotesChanged = do
	newg <- inRepo Git.Config.reRead
	Annex.changeState $ \s -> s 
		{ Annex.remotes = []
		, Annex.gitremotes = Nothing
		, Annex.repo = newg
		}

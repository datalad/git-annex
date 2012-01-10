{- git-annex remote list
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.List where

import qualified Data.Map as M

import Common.Annex
import qualified Annex
import Logs.Remote
import Types.Remote
import Annex.UUID
import Config

import qualified Remote.Git
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Web
import qualified Remote.Hook

remoteTypes :: [RemoteType]
remoteTypes =
	[ Remote.Git.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Web.remote
	, Remote.Hook.remote
	]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached. -}
remoteList :: Annex [Remote]
remoteList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then do
			m <- readRemoteLog
			rs' <- concat <$> mapM (process m) remoteTypes
			Annex.changeState $ \s -> s { Annex.remotes = rs' }
			return rs'
		else return rs
	where
		process m t = enumerate t >>= mapM (gen m t)
		gen m t r = do
			u <- getRepoUUID r
			generate t r u (M.lookup u m)

{- All remotes that are not ignored. -}
enabledRemoteList :: Annex [Remote]
enabledRemoteList = filterM (repoNotIgnored . repo) =<< remoteList

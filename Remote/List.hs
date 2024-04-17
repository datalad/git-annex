{- git-annex remote list
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.List where

import qualified Data.Map as M

import Annex.Common
import qualified Annex
import Logs.Remote
import Types.Remote
import Types.RemoteState
import Annex.UUID
import Remote.Helper.Hooks
import Remote.Helper.ReadOnly
import Remote.Helper.ExportImport
import qualified Git

import qualified Remote.Git
import qualified Remote.GCrypt
import qualified Remote.P2P
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Web
import qualified Remote.BitTorrent
import qualified Remote.WebDAV
import qualified Remote.Adb
import qualified Remote.Tahoe
import qualified Remote.Glacier
import qualified Remote.Ddar
import qualified Remote.GitLFS
import qualified Remote.HttpAlso
import qualified Remote.Borg
import qualified Remote.Rclone
import qualified Remote.Hook
import qualified Remote.External

remoteTypes :: [RemoteType]
remoteTypes = map adjustExportImportRemoteType
	[ Remote.Git.remote
	, Remote.GCrypt.remote
	, Remote.P2P.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Web.remote
	, Remote.BitTorrent.remote
	, Remote.WebDAV.remote
	, Remote.Adb.remote
	, Remote.Tahoe.remote
	, Remote.Glacier.remote
	, Remote.Ddar.remote
	, Remote.GitLFS.remote
	, Remote.HttpAlso.remote
	, Remote.Borg.remote
	, Remote.Rclone.remote
	, Remote.Hook.remote
	, Remote.External.remote
	]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached. -}
remoteList :: Annex [Remote]
remoteList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then remoteList' False
		else return rs

remoteList' :: Bool -> Annex [Remote]
remoteList' autoinit = do
	m <- remoteConfigMap
	rs <- concat <$> mapM (process m) remoteTypes
	Annex.changeState $ \s -> s { Annex.remotes = rs }
	return rs
  where
	process m t = enumerate t autoinit 
		>>= mapM (remoteGen m t) 
		>>= return . catMaybes

{- Generates a Remote. -}
remoteGen :: M.Map UUID RemoteConfig -> RemoteType -> Git.Repo -> Annex (Maybe Remote)
remoteGen m t g = do
	u <- getRepoUUID g
	gc <- Annex.getRemoteGitConfig g
	let cu = fromMaybe u $ remoteAnnexConfigUUID gc
	let rs = RemoteStateHandle cu
	let c = fromMaybe M.empty $ M.lookup cu m
	generate t g u c gc rs >>= \case
		Nothing -> return Nothing
		Just r -> Just <$> adjustExportImport (adjustReadOnly (addHooks r)) rs

{- Updates a local git Remote, re-reading its git config. -}
updateRemote :: Remote -> Annex (Maybe Remote)
updateRemote remote = do
	m <- remoteConfigMap
	remote' <- updaterepo =<< getRepo remote
	remoteGen m (remotetype remote) remote'
  where
	updaterepo r
		| Git.repoIsLocal r || Git.repoIsLocalUnknown r =
			Remote.Git.configRead False r
		| otherwise = return r

{- Checks if a remote is syncable using git. -}
gitSyncableRemoteType :: RemoteType -> Bool
gitSyncableRemoteType t = t `elem`
	[ Remote.Git.remote
	, Remote.GCrypt.remote
	, Remote.P2P.remote
	, Remote.GitLFS.remote
	]

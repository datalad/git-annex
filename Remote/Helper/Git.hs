{- Utilities for git remotes.
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Helper.Git where

import Annex.Common
import qualified Git
import qualified Git.GCrypt
import Types.Availability
import qualified Types.Remote as Remote
import qualified Utility.RawFilePath as R
import qualified Git.Config
import Logs.Proxy
import Types.Cluster

import Data.Time.Clock.POSIX
import System.PosixCompat.Files (modificationTime)
import qualified Data.Map as M
import qualified Data.Set as S

repoCheap :: Git.Repo -> Bool
repoCheap = not . Git.repoIsUrl

localpathCalc :: Git.Repo -> Maybe OsPath
localpathCalc r
	| not (Git.repoIsLocal r) && not (Git.repoIsLocalUnknown r) = Nothing
	| otherwise = Just $ Git.repoPath r

{- Checks relatively inexpensively if a repository is available for use. -}
repoAvail :: Git.Repo -> Annex Availability
repoAvail r 
	| Git.repoIsHttp r = return GloballyAvailable
	| Git.GCrypt.isEncrypted r = do
		g <- gitRepo
		liftIO $ do
			er <- Git.GCrypt.encryptedRemote g r
			if Git.repoIsLocal er || Git.repoIsLocalUnknown er
				then checklocal er
				else return GloballyAvailable
	| Git.repoIsUrl r = return GloballyAvailable
	| Git.repoIsLocalUnknown r = return Unavailable
	| otherwise = checklocal r
  where
	checklocal r' = ifM (liftIO $ isJust <$> catchMaybeIO (Git.Config.read r'))
		( return LocallyAvailable
		, return Unavailable
		)

{- Avoids performing an action on a local repository that's not usable.
 - Does not check that the repository is still available on disk. -}
guardUsable :: Git.Repo -> Annex a -> Annex a -> Annex a
guardUsable r fallback a
	| Git.repoIsLocalUnknown r = fallback
	| otherwise = a

gitRepoInfo :: Remote -> Annex [(String, String)]
gitRepoInfo r = do
	d <- fromRepo Git.localGitDir
	let refsdir = d </> literalOsPath "refs" 
		</> literalOsPath "remotes" 
		</> toOsPath (Remote.name r)
	mtimes <- liftIO $ mapM (\p -> modificationTime <$> R.getFileStatus (fromOsPath p))
		=<< emptyWhenDoesNotExist (dirContentsRecursive refsdir)
	let lastsynctime = case mtimes of
		[] -> "never"
		_ -> show $ posixSecondsToUTCTime $ realToFrac $ maximum mtimes
	repo <- Remote.getRepo r
	let proxied = Git.Config.boolConfig $ isJust $
		remoteAnnexProxiedBy (Remote.gitconfig r)
	proxies <- getProxies
	let proxying = S.toList $ fromMaybe mempty $
		M.lookup (Remote.uuid r) proxies
	let iscluster = isClusterUUID . proxyRemoteUUID
	let proxyname p = Remote.name r ++ "-" ++ proxyRemoteName p
	let proxynames = map proxyname $ filter (not . iscluster) proxying
	let clusternames = map proxyname $ filter iscluster proxying
	return $ catMaybes
		[ Just ("repository location", Git.repoLocation repo)
		, Just ("last synced", lastsynctime)
		, Just ("proxied", proxied)
		, if isClusterUUID (Remote.uuid r)
			then Just ("cluster", Git.Config.boolConfig True)
			else Nothing
		, if null clusternames
			then Nothing
			else Just ("gateway to cluster", unwords clusternames)
		, if null proxynames
			then Nothing
			else Just ("proxying", unwords proxynames)
		]

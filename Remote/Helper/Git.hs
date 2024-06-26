{- Utilities for git remotes.
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.Git where

import Annex.Common
import qualified Git
import qualified Git.GCrypt
import Types.Availability
import qualified Types.Remote as Remote
import qualified Utility.RawFilePath as R
import qualified Git.Config

import Data.Time.Clock.POSIX
import System.PosixCompat.Files (modificationTime)

repoCheap :: Git.Repo -> Bool
repoCheap = not . Git.repoIsUrl

localpathCalc :: Git.Repo -> Maybe FilePath
localpathCalc r
	| not (Git.repoIsLocal r) && not (Git.repoIsLocalUnknown r) = Nothing
	| otherwise = Just $ fromRawFilePath $ Git.repoPath r

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
	d <- fromRawFilePath <$> fromRepo Git.localGitDir
	mtimes <- liftIO $ mapM (\p -> modificationTime <$> R.getFileStatus (toRawFilePath p))
		=<< emptyWhenDoesNotExist (dirContentsRecursive (d </> "refs" </> "remotes" </> Remote.name r))
	let lastsynctime = case mtimes of
		[] -> "never"
		_ -> show $ posixSecondsToUTCTime $ realToFrac $ maximum mtimes
	repo <- Remote.getRepo r
	return
		[ ("repository location", Git.repoLocation repo)
		, ("proxied", Git.Config.boolConfig 
			(isJust (remoteAnnexProxiedBy (Remote.gitconfig r))))
		, ("last synced", lastsynctime)
		]

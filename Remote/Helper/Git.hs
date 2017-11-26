{- Utilities for git remotes.
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Git where

import Annex.Common
import qualified Git
import Types.Availability
import qualified Types.Remote as Remote

import Data.Time.Clock.POSIX

repoCheap :: Git.Repo -> Bool
repoCheap = not . Git.repoIsUrl

localpathCalc :: Git.Repo -> Maybe FilePath
localpathCalc r
	| availabilityCalc r == GloballyAvailable = Nothing
	| otherwise = Just $ Git.repoPath r

availabilityCalc :: Git.Repo -> Availability
availabilityCalc r
	| (Git.repoIsLocal r || Git.repoIsLocalUnknown r) = LocallyAvailable
	| otherwise = GloballyAvailable

{- Avoids performing an action on a local repository that's not usable.
 - Does not check that the repository is still available on disk. -}
guardUsable :: Git.Repo -> Annex a -> Annex a -> Annex a
guardUsable r fallback a
	| Git.repoIsLocalUnknown r = fallback
	| otherwise = a

gitRepoInfo :: Remote -> Annex [(String, String)]
gitRepoInfo r = do
	d <- fromRepo Git.localGitDir
	mtimes <- liftIO $ mapM (modificationTime <$$> getFileStatus)
		=<< dirContentsRecursive (d </> "refs" </> "remotes" </> Remote.name r)
	let lastsynctime = case mtimes of
		[] -> "never"
		_ -> show $ posixSecondsToUTCTime $ realToFrac $ maximum mtimes
	return
		[ ("repository location", Git.repoLocation (Remote.repo r))
		, ("last synced", lastsynctime)
		]

{- git cat-file interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.CatFile (
	catFile,
	catObject,
	catObjectDetails,
	catFileHandle,
	catKey,
	catKeyFile,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Common.Annex
import qualified Git
import qualified Git.CatFile
import qualified Annex
import Git.Types
import Git.FilePath

catFile :: Git.Branch -> FilePath -> Annex L.ByteString
catFile branch file = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catFile h branch file

catObject :: Git.Ref -> Annex L.ByteString
catObject ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObject h ref

catObjectDetails :: Git.Ref -> Annex (Maybe (L.ByteString, Sha))
catObjectDetails ref = do
	h <- catFileHandle
	liftIO $ Git.CatFile.catObjectDetails h ref

{- There can be multiple index files, and a different cat-file is needed
 - for each. This is selected by setting GIT_INDEX_FILE in the gitEnv. -}
catFileHandle :: Annex Git.CatFile.CatFileHandle
catFileHandle = do
	m <- Annex.getState Annex.catfilehandles
	indexfile <- fromMaybe "" . maybe Nothing (lookup "GIT_INDEX_FILE")
		<$> fromRepo gitEnv
	case M.lookup indexfile m of
		Just h -> return h
		Nothing -> do
			h <- inRepo Git.CatFile.catFileStart
			let m' = M.insert indexfile h m
			Annex.changeState $ \s -> s { Annex.catfilehandles = m' }
			return h

{- From the Sha or Ref of a symlink back to the key. -}
catKey :: Ref -> Annex (Maybe Key)
catKey ref = do
	l <- fromInternalGitPath . encodeW8 . L.unpack <$> catObject ref
	return $ if isLinkToAnnex l
		then fileKey $ takeFileName l
		else Nothing

{- From a file in the repository back to the key.
 -
 - Prefixing the file with ./ makes this work even if in a subdirectory
 - of a repo.
 -
 - Ideally, this should reflect the key that's staged in the index,
 - not the key that's committed to HEAD. Unfortunately, git cat-file
 - does not refresh the index file after it's started up, so things
 - newly staged in the index won't show up. It does, however, notice
 - when branches change.
 -
 - For command-line git-annex use, that doesn't matter. It's perfectly
 - reasonable for things staged in the index after the currently running
 - git-annex process to not be noticed by it.
 -
 - For the assistant, this is much more of a problem, since it commits
 - files and then needs to be able to immediately look up their keys.
 - OTOH, the assistant doesn't keep changes staged in the index for very
 - long at all before committing them -- and it won't look at the keys
 - of files until after committing them.
 -
 - So, this gets info from the index, unless running as a daemon.
 -}
catKeyFile :: FilePath -> Annex (Maybe Key)
catKeyFile f = ifM (Annex.getState Annex.daemon)
	( catKey $ Ref $ "HEAD:./" ++ f
	, catKey $ Ref $ ":./" ++ f
	)

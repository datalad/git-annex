{- git-annex links to content
 -
 - On file systems that support them, symlinks are used.
 -
 - On other filesystems, git instead stores the symlink target in a regular
 - file.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Link where

import Common.Annex
import qualified Annex
import qualified Git.HashObject
import qualified Git.UpdateIndex
import qualified Annex.Queue
import Git.Types
import Git.FilePath

type LinkTarget = String

{- Checks if a file is a link to a key. -}
isAnnexLink :: FilePath -> Annex (Maybe Key)
isAnnexLink file = maybe Nothing (fileKey . takeFileName) <$> getAnnexLinkTarget file

{- Gets the link target of a symlink.
 -
 - On a filesystem that does not support symlinks, fall back to getting the
 - link target by looking inside the file.
 -
 - Returns Nothing if the file is not a symlink, or not a link to annex
 - content.
 -}
getAnnexLinkTarget :: FilePath -> Annex (Maybe LinkTarget)
getAnnexLinkTarget file = ifM (coreSymlinks <$> Annex.getGitConfig)
	( check readSymbolicLink $
		return Nothing
	, check readSymbolicLink $
		check probefilecontent $
			return Nothing
	)
  where
	check getlinktarget fallback = do
		v <- liftIO $ catchMaybeIO $ getlinktarget file
		case v of
			Just l
				| isLinkToAnnex (fromInternalGitPath l) -> return v
				| otherwise -> return Nothing
			Nothing -> fallback

	probefilecontent f = do
		h <- openFile f ReadMode
		fileEncoding h
		-- The first 8k is more than enough to read; link
		-- files are small.
		s <- take 8192 <$> hGetContents h
		-- If we got the full 8k, the file is too large
		if length s == 8192
			then do
				hClose h
				return ""
			else do
				hClose h
				-- If there are any NUL or newline
				-- characters, or whitespace, we
				-- certianly don't have a link to a
				-- git-annex key.
				return $ if any (`elem` s) "\0\n\r \t"
					then ""
					else s

{- Creates a link on disk.
 -
 - On a filesystem that does not support symlinks, writes the link target
 - to a file. Note that git will only treat the file as a symlink if
 - it's staged as such, so use addAnnexLink when adding a new file or
 - modified link to git.
 -}
makeAnnexLink :: LinkTarget -> FilePath -> Annex ()
makeAnnexLink linktarget file = ifM (coreSymlinks <$> Annex.getGitConfig)
	( liftIO $ do
		void $ tryIO $ removeFile file
		createSymbolicLink linktarget file
	, liftIO $ writeFile file linktarget
	)

{- Creates a link on disk, and additionally stages it in git. -}
addAnnexLink :: LinkTarget -> FilePath -> Annex ()
addAnnexLink linktarget file = do
	makeAnnexLink linktarget file
	stageSymlink file =<< hashSymlink linktarget

{- Injects a symlink target into git, returning its Sha. -}
hashSymlink :: LinkTarget -> Annex Sha
hashSymlink linktarget = inRepo $ Git.HashObject.hashObject BlobObject $ 
	toInternalGitPath linktarget

{- Stages a symlink to the annex, using a Sha of its target. -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)

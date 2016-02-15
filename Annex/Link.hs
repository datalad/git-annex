{- git-annex links to content
 -
 - On file systems that support them, symlinks are used.
 -
 - On other filesystems, git instead stores the symlink target in a regular
 - file.
 -
 - Pointer files are used instead of symlinks for unlocked files.
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Link where

import Annex.Common
import qualified Annex
import qualified Git.HashObject
import qualified Git.UpdateIndex
import qualified Annex.Queue
import Git.Types
import Git.FilePath

import qualified Data.ByteString.Lazy as L

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
getAnnexLinkTarget f = getAnnexLinkTarget' f
	=<< (coreSymlinks <$> Annex.getGitConfig)

{- Pass False to force looking inside file. -}
getAnnexLinkTarget' :: FilePath -> Bool -> Annex (Maybe LinkTarget)
getAnnexLinkTarget' file coresymlinks = if coresymlinks
	then check readSymbolicLink $
		return Nothing
	else check readSymbolicLink $
		check probefilecontent $
			return Nothing
  where
	check getlinktarget fallback = do
		v <- liftIO $ catchMaybeIO $ getlinktarget file
		case v of
			Just l
				| isLinkToAnnex (fromInternalGitPath l) -> return v
				| otherwise -> return Nothing
			Nothing -> fallback

	probefilecontent f = withFile f ReadMode $ \h -> do
		fileEncoding h
		-- The first 8k is more than enough to read; link
		-- files are small.
		s <- take 8192 <$> hGetContents h
		-- If we got the full 8k, the file is too large
		if length s == 8192
			then return ""
			else 
				-- If there are any NUL or newline
				-- characters, or whitespace, we
				-- certianly don't have a link to a
				-- git-annex key.
				return $ if any (`elem` s) "\0\n\r \t"
					then ""
					else s

makeAnnexLink :: LinkTarget -> FilePath -> Annex ()
makeAnnexLink = makeGitLink

{- Creates a link on disk.
 -
 - On a filesystem that does not support symlinks, writes the link target
 - to a file. Note that git will only treat the file as a symlink if
 - it's staged as such, so use addAnnexLink when adding a new file or
 - modified link to git.
 -}
makeGitLink :: LinkTarget -> FilePath -> Annex ()
makeGitLink linktarget file = ifM (coreSymlinks <$> Annex.getGitConfig)
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

hashSymlink' :: Git.HashObject.HashObjectHandle -> LinkTarget -> Annex Sha
hashSymlink' h linktarget = liftIO $ Git.HashObject.hashBlob h $
	toInternalGitPath linktarget

{- Stages a symlink to an annexed object, using a Sha of its target. -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)

{- Injects a pointer file content into git, returning its Sha. -}
hashPointerFile :: Key -> Annex Sha
hashPointerFile key = inRepo $ Git.HashObject.hashObject BlobObject $
	formatPointer key

{- Stages a pointer file, using a Sha of its content -}
stagePointerFile :: FilePath -> Sha -> Annex ()
stagePointerFile file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageFile sha FileBlob file)

{- Parses a symlink target or a pointer file to a Key.
 - Only looks at the first line, as pointer files can have subsequent
 - lines. -}
parseLinkOrPointer :: L.ByteString -> Maybe Key
parseLinkOrPointer = parseLinkOrPointer' . decodeBS . L.take maxsz
  where
	{- Want to avoid buffering really big files in git into
	 - memory when reading files that may be pointers.
	 -
	 - 8192 bytes is plenty for a pointer to a key.
	 - Pad some more to allow for any pointer files that might have
	 - lines after the key explaining what the file is used for. -}
	maxsz = 81920

parseLinkOrPointer' :: String -> Maybe Key
parseLinkOrPointer' = go . fromInternalGitPath . takeWhile (not . lineend)
  where
	go l
		| isLinkToAnnex l = file2key $ takeFileName l
		| otherwise = Nothing
	lineend '\n' = True
	lineend '\r' = True
	lineend _ = False

formatPointer :: Key -> String
formatPointer k = 
	toInternalGitPath (pathSeparator:objectDir </> key2file k) ++ "\n"

{- Checks if a file is a pointer to a key. -}
isPointerFile :: FilePath -> IO (Maybe Key)
isPointerFile f = catchDefaultIO Nothing $ 
	parseLinkOrPointer <$> L.readFile f

{- Checks a symlink target or pointer file first line to see if it
 - appears to point to annexed content.
 -
 - We only look for paths inside the .git directory, and not at the .git
 - directory itself, because GIT_DIR may cause a directory name other
 - than .git to be used.
 -}
isLinkToAnnex :: FilePath -> Bool
isLinkToAnnex s = (pathSeparator:objectDir) `isInfixOf` s
#ifdef mingw32_HOST_OS
	-- '/' is still used inside pointer files on Windows, not the native
	-- '\'
	|| ('/':objectDir) `isInfixOf` s
#endif

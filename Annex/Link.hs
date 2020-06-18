{- git-annex links to content
 -
 - On file systems that support them, symlinks are used.
 -
 - On other filesystems, git instead stores the symlink target in a regular
 - file.
 -
 - Pointer files are used instead of symlinks for unlocked files.
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Annex.Link where

import Annex.Common
import qualified Annex
import qualified Annex.Queue
import qualified Git.Queue
import qualified Git.UpdateIndex
import qualified Git.Index
import qualified Git.LockFile
import qualified Git.Env
import qualified Git
import Git.Types
import Git.FilePath
import Git.Config
import Annex.HashObject
import Annex.InodeSentinal
import Annex.GitOverlay
import Utility.FileMode
import Utility.InodeCache
import Utility.Tmp.Dir
import Utility.CopyFile
import qualified Utility.RawFilePath as R

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P

type LinkTarget = String

{- Checks if a file is a link to a key. -}
isAnnexLink :: RawFilePath -> Annex (Maybe Key)
isAnnexLink file = maybe Nothing parseLinkTargetOrPointer <$> getAnnexLinkTarget file

{- Gets the link target of a symlink.
 -
 - On a filesystem that does not support symlinks, fall back to getting the
 - link target by looking inside the file.
 -
 - Returns Nothing if the file is not a symlink, or not a link to annex
 - content.
 -}
getAnnexLinkTarget :: RawFilePath -> Annex (Maybe S.ByteString)
getAnnexLinkTarget f = getAnnexLinkTarget' f
	=<< (coreSymlinks <$> Annex.getGitConfig)

{- Pass False to force looking inside file, for when git checks out
 - symlinks as plain files. -}
getAnnexLinkTarget' :: RawFilePath -> Bool -> Annex (Maybe S.ByteString)
getAnnexLinkTarget' file coresymlinks = if coresymlinks
	then check probesymlink $
		return Nothing
	else check probesymlink $
		check probefilecontent $
			return Nothing
  where
	check getlinktarget fallback = 
		liftIO (catchMaybeIO getlinktarget) >>= \case
			Just l
				| isLinkToAnnex l -> return (Just l)
				| otherwise -> return Nothing
			Nothing -> fallback

	probesymlink = R.readSymbolicLink file

	probefilecontent = withFile (fromRawFilePath file) ReadMode $ \h -> do
		s <- S.hGet h unpaddedMaxPointerSz
		-- If we got the full amount, the file is too large
		-- to be a symlink target.
		return $ if S.length s == unpaddedMaxPointerSz
			then mempty
			else 
				-- If there are any NUL or newline
				-- characters, or whitespace, we
				-- certianly don't have a symlink to a
				-- git-annex key.
				if any (`S8.elem` s) "\0\n\r \t"
					then mempty
					else s

makeAnnexLink :: LinkTarget -> RawFilePath -> Annex ()
makeAnnexLink = makeGitLink

{- Creates a link on disk.
 -
 - On a filesystem that does not support symlinks, writes the link target
 - to a file. Note that git will only treat the file as a symlink if
 - it's staged as such, so use addAnnexLink when adding a new file or
 - modified link to git.
 -}
makeGitLink :: LinkTarget -> RawFilePath -> Annex ()
makeGitLink linktarget file = ifM (coreSymlinks <$> Annex.getGitConfig)
	( liftIO $ do
		void $ tryIO $ removeFile (fromRawFilePath file)
		createSymbolicLink linktarget (fromRawFilePath file)
	, liftIO $ writeFile (fromRawFilePath file) linktarget
	)

{- Creates a link on disk, and additionally stages it in git. -}
addAnnexLink :: LinkTarget -> RawFilePath -> Annex ()
addAnnexLink linktarget file = do
	makeAnnexLink linktarget file
	stageSymlink file =<< hashSymlink linktarget

{- Injects a symlink target into git, returning its Sha. -}
hashSymlink :: LinkTarget -> Annex Sha
hashSymlink = hashBlob . toInternalGitPath . toRawFilePath

{- Stages a symlink to an annexed object, using a Sha of its target. -}
stageSymlink :: RawFilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink (fromRawFilePath file) sha)

{- Injects a pointer file content into git, returning its Sha. -}
hashPointerFile :: Key -> Annex Sha
hashPointerFile key = hashBlob $ formatPointer key

{- Stages a pointer file, using a Sha of its content -}
stagePointerFile :: RawFilePath -> Maybe FileMode -> Sha -> Annex ()
stagePointerFile file mode sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageFile sha treeitemtype $ fromRawFilePath file)
  where
	treeitemtype
		| maybe False isExecutable mode = TreeExecutable
		| otherwise = TreeFile

writePointerFile :: RawFilePath -> Key -> Maybe FileMode -> IO ()
writePointerFile file k mode = do
	S.writeFile (fromRawFilePath file) (formatPointer k)
	maybe noop (setFileMode $ fromRawFilePath file) mode

newtype Restage = Restage Bool

{- Restage pointer file. This is used after updating a worktree file
 - when content is added/removed, to prevent git status from showing
 - it as modified.
 -
 - Asks git to refresh its index information for the file.
 - That in turn runs the clean filter on the file; when the clean
 - filter produces the same pointer that was in the index before, git
 - realizes that the file has not actually been modified.
 -
 - Note that, if the pointer file is staged for deletion, or has different
 - content than the current worktree content staged, this won't change
 - that. So it's safe to call at any time and any situation.
 -
 - If the index is known to be locked (eg, git add has run git-annex),
 - that would fail. Restage False will prevent the index being updated.
 - Will display a message to help the user understand why
 - the file will appear to be modified.
 -
 - This uses the git queue, so the update is not performed immediately,
 - and this can be run multiple times cheaply.
 -
 - The InodeCache is for the worktree file. It is used to detect when
 - the worktree file is changed by something else before git update-index
 - gets to look at it.
 -}
restagePointerFile :: Restage -> RawFilePath -> InodeCache -> Annex ()
restagePointerFile (Restage False) f _ =
	toplevelWarning True $ unableToRestage $ Just $ fromRawFilePath f
restagePointerFile (Restage True) f orig = withTSDelta $ \tsd ->
	-- Avoid refreshing the index if run by the
	-- smudge clean filter, because git uses that when
	-- it's already refreshing the index, probably because
	-- this very action is running. Running it again would likely
	-- deadlock.
	unlessM (Annex.getState Annex.insmudgecleanfilter) $ do
		-- update-index is documented as picky about "./file" and it
		-- fails on "../../repo/path/file" when cwd is not in the repo 
		-- being acted on. Avoid these problems with an absolute path.
		absf <- liftIO $ absPath $ fromRawFilePath f
		Annex.Queue.addInternalAction runner [(absf, isunmodified tsd)]
  where
	isunmodified tsd = genInodeCache f tsd >>= return . \case
		Nothing -> False
		Just new -> compareStrong orig new

	-- Other changes to the files may have been staged before this
	-- gets a chance to run. To avoid a race with any staging of
	-- changes, first lock the index file. Then run git update-index
	-- on all still-unmodified files, using a copy of the index file,
	-- to bypass the lock. Then replace the old index file with the new
	-- updated index file.
	runner :: Git.Queue.InternalActionRunner Annex
	runner = Git.Queue.InternalActionRunner "restagePointerFile" $ \r l -> do
		realindex <- liftIO $ Git.Index.currentIndexFile r
		let lock = Git.Index.indexFileLock realindex
		    lockindex = liftIO $ catchMaybeIO $ Git.LockFile.openLock' lock
		    unlockindex = liftIO . maybe noop Git.LockFile.closeLock
		    showwarning = warning $ unableToRestage Nothing
		    go Nothing = showwarning
		    go (Just _) = withTmpDirIn (fromRawFilePath $ Git.localGitDir r) "annexindex" $ \tmpdir -> do
			let tmpindex = tmpdir </> "index"
			let updatetmpindex = do
				r' <- liftIO $ Git.Env.addGitEnv r Git.Index.indexEnv 
					=<< Git.Index.indexEnvVal tmpindex
				-- Avoid git warning about CRLF munging.
				let r'' = r' { gitGlobalOpts = gitGlobalOpts r' ++
					[ Param "-c"
					, Param $ "core.safecrlf=" ++ boolConfig False
					] }
				runsGitAnnexChildProcess' r'' $ \r''' ->
					liftIO $ Git.UpdateIndex.refreshIndex r''' $ \feed ->
						forM_ l $ \(f', checkunmodified) ->
							whenM checkunmodified $
								feed f'
			let replaceindex = catchBoolIO $ do
				moveFile tmpindex realindex
				return True
			ok <- liftIO (createLinkOrCopy realindex tmpindex)
				<&&> updatetmpindex
				<&&> liftIO replaceindex
			unless ok showwarning
		bracket lockindex unlockindex go

unableToRestage :: Maybe FilePath -> String
unableToRestage mf = unwords
	[ "git status will show " ++ fromMaybe "some files" mf
	, "to be modified, since content availability has changed"
	, "and git-annex was unable to update the index."
	, "This is only a cosmetic problem affecting git status; git add,"
	, "git commit, etc won't be affected."
	, "To fix the git status display, you can run:"
	, "git update-index -q --refresh " ++ fromMaybe "<file>" mf
	]

{- Parses a symlink target or a pointer file to a Key. -}
parseLinkTargetOrPointer :: S.ByteString -> Maybe Key
parseLinkTargetOrPointer = parseLinkTarget . S8.takeWhile (not . lineend)
  where
	lineend '\n' = True
	lineend '\r' = True
	lineend _ = False

{- Avoid looking at more of the lazy ByteString than necessary since it
 - could be reading from a large file that is not a pointer file. -}
parseLinkTargetOrPointerLazy :: L.ByteString -> Maybe Key
parseLinkTargetOrPointerLazy b = 
	let b' = L.take (fromIntegral maxPointerSz) b
	in parseLinkTargetOrPointer (L.toStrict b')

{- Parses a symlink target to a Key. -}
parseLinkTarget :: S.ByteString -> Maybe Key
parseLinkTarget l
	| isLinkToAnnex l = fileKey $ snd $ S8.breakEnd pathsep l
	| otherwise = Nothing
  where
	pathsep '/' = True
#ifdef mingw32_HOST_OS
	pathsep '\\' = True
#endif
	pathsep _ = False

formatPointer :: Key -> S.ByteString
formatPointer k = prefix <> keyFile k <> nl
  where
	prefix = toInternalGitPath $ P.pathSeparator `S.cons` objectDir'
	nl = S8.singleton '\n'

{- Maximum size of a file that could be a pointer to a key.
 - Check to avoid buffering really big files in git into
 - memory when reading files that may be pointers.
 -
 - 8192 bytes is plenty for a pointer to a key. This adds some additional
 - padding to allow for any pointer files that might have
 - lines after the key explaining what the file is used for. -}
maxPointerSz :: Integer
maxPointerSz = 81920

unpaddedMaxPointerSz :: Int
unpaddedMaxPointerSz = 8192

{- Checks if a worktree file is a pointer to a key.
 -
 - Unlocked files whose content is present are not detected by this. -}
isPointerFile :: RawFilePath -> IO (Maybe Key)
isPointerFile f = catchDefaultIO Nothing $ withFile (fromRawFilePath f) ReadMode $ \h ->
	parseLinkTargetOrPointer <$> S.hGet h unpaddedMaxPointerSz

{- Checks a symlink target or pointer file first line to see if it
 - appears to point to annexed content.
 -
 - We only look for paths inside the .git directory, and not at the .git
 - directory itself, because GIT_DIR may cause a directory name other
 - than .git to be used.
 -}
isLinkToAnnex :: S.ByteString -> Bool
isLinkToAnnex s = p `S.isInfixOf` s
#ifdef mingw32_HOST_OS
	-- '/' is used inside pointer files on Windows, not the native '\'
	|| p' `S.isInfixOf` s
#endif
  where
	p = P.pathSeparator `S.cons` objectDir'
#ifdef mingw32_HOST_OS
	p' = toInternalGitPath p
#endif

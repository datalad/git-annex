{- git-annex file permissions
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Perms (
	FileMode,
	setAnnexFilePerm,
	setAnnexDirPerm,
	resetAnnexFilePerm,
	annexFileMode,
	createAnnexDirectory,
	createWorkTreeDirectory,
	freezeContent,
	freezeContent',
	freezeContent'',
	checkContentWritePerm,
	checkContentWritePerm',
	thawContent,
	thawContent',
	createContentDir,
	freezeContentDir,
	thawContentDir,
	modifyContentDir,
	modifyContentDirWhenExists,
	withShared,
	hasFreezeHook,
	hasThawHook,
) where

import Annex.Common
import Annex.Hook
import Utility.FileMode
import Git
import Git.ConfigTypes
import qualified Annex
import Annex.Version
import Types.RepoVersion
import Config
import Utility.Directory.Create
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (fileMode, intersectFileModes, nullFileMode, groupWriteMode, ownerWriteMode, ownerReadMode, groupReadMode, otherReadMode, stdFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode, setGroupIDMode)

withShared :: (SharedRepository -> Annex a) -> Annex a
withShared a = a =<< coreSharedRepository <$> Annex.getGitConfig

setAnnexFilePerm :: OsPath -> Annex ()
setAnnexFilePerm = setAnnexPerm False

setAnnexDirPerm :: OsPath -> Annex ()
setAnnexDirPerm = setAnnexPerm True

{- Sets appropriate file mode for a file or directory in the annex,
 - other than the content files and content directory. Normally,
 - don't change the mode, but with core.sharedRepository set,
 - allow the group to write, etc. -}
setAnnexPerm :: Bool -> OsPath -> Annex ()
setAnnexPerm isdir file = setAnnexPerm' Nothing isdir >>= \go -> liftIO (go file)

setAnnexPerm' :: Maybe ([FileMode] -> FileMode -> FileMode) -> Bool -> Annex (OsPath -> IO ())
setAnnexPerm' modef isdir = ifM crippledFileSystem
	( return (const noop)
	, withShared $ \s -> return $ \file -> go s file
	)
  where
	go GroupShared file = void $ tryIO $ modifyFileMode file $ modef' $
		groupSharedModes ++
		if isdir then [ ownerExecuteMode, groupExecuteMode ] else []
	go AllShared file = void $ tryIO $ modifyFileMode file $ modef' $
		readModes ++
		[ ownerWriteMode, groupWriteMode ] ++
		if isdir then executeModes else []
	go UnShared file = case modef of
		Nothing -> noop
		Just f -> void $ tryIO $
			modifyFileMode file $ f []
	go (UmaskShared n) file = void $ tryIO $
		R.setFileMode (fromOsPath file) $
			if isdir then umaskSharedDirectory n else n
	modef' = fromMaybe addModes modef

resetAnnexFilePerm :: OsPath -> Annex ()
resetAnnexFilePerm = resetAnnexPerm False

{- Like setAnnexPerm, but ignores the current mode of the file entirely,
 - and sets the same mode that the umask would result in when creating a
 - new file.
 -
 - Useful eg, after creating a temporary file with locked down modes,
 - which is going to be moved to a non-temporary location and needs
 - usual modes.
 -}
resetAnnexPerm :: Bool -> OsPath -> Annex ()
resetAnnexPerm isdir file = unlessM crippledFileSystem $ do
	defmode <- liftIO defaultFileMode
	let modef moremodes _oldmode = addModes moremodes defmode
	setAnnexPerm' (Just modef) isdir >>= \go -> liftIO (go file)

{- Creates a ModeSetter which can be used for creating a file in the annex
 - (other than content files, which are locked down more). -}
annexFileMode :: Annex ModeSetter
annexFileMode = do
	modesetter <- setAnnexPerm' Nothing False
	withShared (\s -> pure $ mk s modesetter)
  where
	mk GroupShared = ModeSetter stdFileMode
	mk AllShared = ModeSetter stdFileMode
	mk UnShared = ModeSetter stdFileMode
	mk (UmaskShared mode) = ModeSetter mode

{- Creates a directory inside the gitAnnexDir (or possibly the dbdir), 
 - creating any parent directories up to and including the gitAnnexDir.
 - Makes directories with appropriate permissions. -}
createAnnexDirectory :: OsPath -> Annex ()
createAnnexDirectory dir = do
	top <- parentDir <$> fromRepo gitAnnexDir
	tops <- annexDbDir <$> Annex.getGitConfig >>= return . \case
		Nothing -> [top]
		Just dbdir -> [top, parentDir (parentDir dbdir)]
	createDirectoryUnder' tops dir createdir
  where
	createdir p = do
		liftIO $ createDirectory p
		setAnnexDirPerm p

{- Create a directory in the git work tree, creating any parent
 - directories up to the top of the work tree.
 -
 - Uses default permissions.
 -}
createWorkTreeDirectory :: OsPath -> Annex ()
createWorkTreeDirectory dir = do
	fromRepo repoWorkTree >>= liftIO . \case
		Just wt -> createDirectoryUnder [wt] dir
		-- Should never happen, but let whatever tries to write
		-- to the directory be what throws an exception, as that
		-- will be clearer than an exception from here.
		Nothing -> noop

{- Normally, blocks writing to an annexed file, and modifies file
 - permissions to allow reading it.
 -
 - Before v9, when core.sharedRepository is set, the write bits are not
 - removed from the file, but instead the appropriate group write bits
 - are set. This is necessary to let other users in the group lock the file.
 - v9 improved this by using separate lock files, so the content file does
 - not need to be writable when using it.
 -
 - In a shared repository, the current user may not be able to change
 - a file owned by another user, so failure to change modes is ignored.
 -
 - Note that, on Linux, xattrs can sometimes prevent removing
 - certain permissions from a file with chmod. (Maybe some ACLs too?) 
 - In such a case, this will return with the file still having some mode
 - it should not normally have. checkContentWritePerm can detect when
 - that happens with write permissions.
 -}
freezeContent :: OsPath -> Annex ()
freezeContent file = withShared $ \sr -> void $ freezeContent' sr file

freezeContent' :: SharedRepository -> OsPath -> Annex HookResult
freezeContent' sr file = freezeContent'' sr file =<< getVersion

freezeContent'' :: SharedRepository -> OsPath -> Maybe RepoVersion -> Annex HookResult
freezeContent'' sr file rv = do
	fastDebug "Annex.Perms" ("freezing content " ++ fromOsPath file)
	unlessM crippledFileSystem $ go sr
	freezeHook file
  where
	go UnShared = liftIO $ nowriteadd [ownerReadMode]
	go GroupShared = if versionNeedsWritableContentFiles rv
		then liftIO $ ignoresharederr $ modmode $ addModes
			[ownerReadMode, groupReadMode, ownerWriteMode, groupWriteMode]
		else liftIO $ ignoresharederr $
			nowriteadd [ownerReadMode, groupReadMode]
	go AllShared = if versionNeedsWritableContentFiles rv
		then liftIO $ ignoresharederr $ modmode $ addModes
			(readModes ++ writeModes)
		else liftIO $ ignoresharederr $ 
			nowriteadd readModes
	go (UmaskShared n) = if versionNeedsWritableContentFiles rv
		-- Assume that the configured mode includes write bits
		-- for all users who should be able to lock the file, so
		-- don't need to add any write modes.
		then liftIO $ ignoresharederr $ modmode $ const n
		else liftIO $ ignoresharederr $ modmode $ const $
			removeModes writeModes n

	ignoresharederr = void . tryIO

	modmode = modifyFileMode file

	nowriteadd readmodes = modmode $ 
		removeModes writeModes .
		addModes readmodes

{- Checks if the write permissions are as freezeContent should set them.
 -
 - When the repository is shared, the user may not be able to change
 - permissions of a file owned by another user. So if the permissions seem
 - wrong, but the repository is shared, returns Nothing. If the permissions
 - are wrong otherwise, returns Just False.
 -
 - When there is a freeze hook, it may prevent write in some way other than
 - permissions. One use of a freeze hook is when the filesystem does not
 - support removing write permissions, so when there is such a hook
 - write permissions are ignored.
 -}
checkContentWritePerm :: OsPath -> Annex (Maybe Bool)
checkContentWritePerm file = ifM crippledFileSystem
	( return (Just True)
	, do
		rv <- getVersion
		hasfreezehook <- hasFreezeHook
		withShared $ \sr ->
			liftIO $ checkContentWritePerm' sr file rv hasfreezehook
	)

checkContentWritePerm' :: SharedRepository -> OsPath -> Maybe RepoVersion -> Bool -> IO (Maybe Bool)
checkContentWritePerm' sr file rv hasfreezehook
	| hasfreezehook = return (Just True)
	| otherwise = case sr of
		UnShared -> want Just (excludemodes writeModes)
		GroupShared
			| versionNeedsWritableContentFiles rv -> want sharedret
				(includemodes [ownerWriteMode, groupWriteMode])
			| otherwise -> want sharedret (excludemodes writeModes)
		AllShared
			| versionNeedsWritableContentFiles rv -> 
				want sharedret (includemodes writeModes)
			| otherwise -> want sharedret (excludemodes writeModes)
		UmaskShared n
			| versionNeedsWritableContentFiles rv -> want sharedret
				(\havemode -> havemode == n)
			| otherwise -> want sharedret
				(\havemode -> havemode == removeModes writeModes n)
  where
	want mk f = catchMaybeIO (fileMode <$> R.getFileStatus (fromOsPath file))
		>>= return . \case
			Just havemode -> mk (f havemode)
			Nothing -> mk True
	
	includemodes l havemode = havemode == combineModes (havemode:l)
	excludemodes l havemode = all (\m -> intersectFileModes m havemode == nullFileMode) l

	sharedret True = Just True
	sharedret False = Nothing

{- Allows writing to an annexed file that freezeContent was called on
 - before. -}
thawContent :: OsPath -> Annex ()
thawContent file = withShared $ \sr -> void $ thawContent' sr file

thawContent' :: SharedRepository -> OsPath -> Annex HookResult
thawContent' sr file = do
	fastDebug "Annex.Perms" ("thawing content " ++ fromOsPath file)
	thawPerms (go sr) (thawHook file)
  where
	go GroupShared = liftIO $ void $ tryIO $ groupWriteRead file
	go AllShared = liftIO $ void $ tryIO $ groupWriteRead file
	go UnShared = liftIO $ allowWrite file
	go (UmaskShared n) = liftIO $ void $ tryIO $
		R.setFileMode (fromOsPath file) n

{- Runs an action that thaws a file's permissions. This will probably
 - fail on a crippled filesystem. But, if file modes are supported on a
 - crippled filesystem, the file may be frozen, so try to thaw its
 - permissions. -}
thawPerms :: Annex () -> Annex HookResult -> Annex HookResult
thawPerms a hook = ifM crippledFileSystem
	( void (tryNonAsync a) `after` hook
	, a `after` hook
	)

{- Blocks writing to the directory an annexed file is in, to prevent the
 - file accidentally being deleted. However, if core.sharedRepository
 - is set, this is not done, since the group must be allowed to delete the
 - file without being able to thaw the directory.
 -}
freezeContentDir :: OsPath -> Annex ()
freezeContentDir file = do
	fastDebug "Annex.Perms" ("freezing content directory " ++ fromOsPath dir)
	unlessM crippledFileSystem $ withShared go
	void $ freezeHook dir
  where
	dir = parentDir file
	go UnShared = liftIO $ preventWrite dir
	go GroupShared = liftIO $ void $ tryIO $ groupWriteRead dir
	go AllShared = liftIO $ void $ tryIO $ groupWriteRead dir
	go (UmaskShared n) = liftIO $ void $ tryIO $ R.setFileMode (fromOsPath dir) $
		umaskSharedDirectory $ 
			-- If n includes group or other write mode, leave
			-- them set to allow them to delete the file without
			-- being able to thaw the directory.
			removeModes [ownerWriteMode] n

thawContentDir :: OsPath -> Annex ()
thawContentDir file = do
	fastDebug "Annex.Perms" ("thawing content directory " ++ fromOsPath dir)
	void $ thawPerms (withShared (liftIO . go)) (thawHook dir)
  where
	dir = parentDir file
	go UnShared = allowWrite dir
	go GroupShared = allowWrite dir
	go AllShared = allowWrite dir
	go (UmaskShared n) = R.setFileMode (fromOsPath dir) n

{- Makes the directory tree to store an annexed file's content,
 - with appropriate permissions on each level. -}
createContentDir :: OsPath -> Annex ()
createContentDir dest = do
	unlessM (liftIO $ doesDirectoryExist dir) $
		createAnnexDirectory dir 
	-- might have already existed with restricted perms
	void $ thawHook dir
	unlessM crippledFileSystem $ liftIO $ allowWrite dir
  where
	dir = parentDir dest

{- Creates the content directory for a file if it doesn't already exist,
 - or thaws it if it does, then runs an action to modify a file in the
 - directory, and finally, freezes the content directory. -}
modifyContentDir :: OsPath -> Annex a -> Annex a
modifyContentDir f a = do
	createContentDir f -- also thaws it
	v <- tryNonAsync a
	freezeContentDir f
	either throwM return v

{- Like modifyContentDir, but avoids creating the content directory if it
 - does not already exist. In that case, the action will probably fail. -}
modifyContentDirWhenExists :: OsPath -> Annex a -> Annex a
modifyContentDirWhenExists f a = do
	thawContentDir f
	v <- tryNonAsync a
	freezeContentDir f
	either throwM return v

hasFreezeHook :: Annex Bool
hasFreezeHook = 
	(isJust . annexFreezeContentCommand <$> Annex.getGitConfig)
		<||>
	(doesAnnexHookExist freezeContentAnnexHook)

hasThawHook :: Annex Bool
hasThawHook =
	(isJust . annexThawContentCommand <$> Annex.getGitConfig)
		<||>
	(doesAnnexHookExist thawContentAnnexHook)

freezeHook :: OsPath -> Annex HookResult
freezeHook = runAnnexPathHook "%path"
	freezeContentAnnexHook annexFreezeContentCommand

thawHook :: OsPath -> Annex HookResult
thawHook = runAnnexPathHook "%path"
	thawContentAnnexHook annexThawContentCommand

{- Calculate mode to use for a directory from the mode to use for a file.
 -
 - This corresponds to git's handling of core.sharedRepository=0xxx
 -}
umaskSharedDirectory :: FileMode -> FileMode
umaskSharedDirectory n = flip addModes n $ map snd $ filter fst
	[ (isset ownerReadMode, ownerExecuteMode)
	, (isset groupReadMode, groupExecuteMode)
	, (isset otherReadMode, otherExecuteMode)
	, (isset groupReadMode || isset groupWriteMode, setGroupIDMode)
	]
  where
	isset v = checkMode v n

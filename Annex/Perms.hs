{- git-annex file permissions
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Perms (
	FileMode,
	setAnnexFilePerm,
	setAnnexDirPerm,
	resetAnnexFilePerm,
	annexFileMode,
	createAnnexDirectory,
	createWorkTreeDirectory,
	noUmask,
	freezeContent,
	isContentWritePermOk,
	thawContent,
	chmodContent,
	createContentDir,
	freezeContentDir,
	thawContentDir,
	modifyContent,
	withShared,
) where

import Annex.Common
import Utility.FileMode
import Git
import Git.ConfigTypes
import qualified Annex
import Config
import Utility.Directory.Create
import qualified Utility.RawFilePath as R

withShared :: (SharedRepository -> Annex a) -> Annex a
withShared a = a =<< coreSharedRepository <$> Annex.getGitConfig

setAnnexFilePerm :: RawFilePath -> Annex ()
setAnnexFilePerm = setAnnexPerm False

setAnnexDirPerm :: RawFilePath -> Annex ()
setAnnexDirPerm = setAnnexPerm True

{- Sets appropriate file mode for a file or directory in the annex,
 - other than the content files and content directory. Normally,
 - don't change the mode, but with core.sharedRepository set,
 - allow the group to write, etc. -}
setAnnexPerm :: Bool -> RawFilePath -> Annex ()
setAnnexPerm = setAnnexPerm' Nothing

setAnnexPerm' :: Maybe ([FileMode] -> FileMode -> FileMode) -> Bool -> RawFilePath -> Annex ()
setAnnexPerm' modef isdir file = unlessM crippledFileSystem $
	withShared $ liftIO . go
  where
	go GroupShared = void $ tryIO $ modifyFileMode file $ modef' $
		groupSharedModes ++
		if isdir then [ ownerExecuteMode, groupExecuteMode ] else []
	go AllShared = void $ tryIO $ modifyFileMode file $ modef' $
		readModes ++
		[ ownerWriteMode, groupWriteMode ] ++
		if isdir then executeModes else []
	go _ = case modef of
		Nothing -> noop
		Just f -> void $ tryIO $
			modifyFileMode file $ f []
	modef' = fromMaybe addModes modef

resetAnnexFilePerm :: RawFilePath -> Annex ()
resetAnnexFilePerm = resetAnnexPerm False

{- Like setAnnexPerm, but ignores the current mode of the file entirely,
 - and sets the same mode that the umask would result in when creating a
 - new file.
 -
 - Useful eg, after creating a temporary file with locked down modes,
 - which is going to be moved to a non-temporary location and needs
 - usual modes.
 -}
resetAnnexPerm :: Bool -> RawFilePath -> Annex ()
resetAnnexPerm isdir file = unlessM crippledFileSystem $ do
	defmode <- liftIO defaultFileMode
	let modef moremodes _oldmode = addModes moremodes defmode
	setAnnexPerm' (Just modef) isdir file

{- Gets the appropriate mode to use for creating a file in the annex
 - (other than content files, which are locked down more). The umask is not
 - taken into account; this is for use with actions that create the file
 - and apply the umask automatically. -}
annexFileMode :: Annex FileMode
annexFileMode = withShared $ return . go
  where
	go GroupShared = sharedmode
	go AllShared = combineModes (sharedmode:readModes)
	go _ = stdFileMode
	sharedmode = combineModes groupSharedModes

{- Creates a directory inside the gitAnnexDir, creating any parent
 - directories up to and including the gitAnnexDir.
 - Makes directories with appropriate permissions. -}
createAnnexDirectory :: RawFilePath -> Annex ()
createAnnexDirectory dir = do
	top <- parentDir <$> fromRepo gitAnnexDir
	createDirectoryUnder' top dir createdir
  where
	createdir p = do
		liftIO $ R.createDirectory p
		setAnnexDirPerm p

{- Create a directory in the git work tree, creating any parent
 - directories up to the top of the work tree.
 -
 - Uses default permissions.
 -}
createWorkTreeDirectory :: RawFilePath -> Annex ()
createWorkTreeDirectory dir = do
	fromRepo repoWorkTree >>= liftIO . \case
		Just wt -> createDirectoryUnder wt dir
		-- Should never happen, but let whatever tries to write
		-- to the directory be what throws an exception, as that
		-- will be clearer than an exception from here.
		Nothing -> noop

{- Normally, blocks writing to an annexed file, and modifies file
 - permissions to allow reading it.
 -
 - When core.sharedRepository is set, the write bits are not removed from
 - the file, but instead the appropriate group write bits are set. This is
 - necessary to let other users in the group lock the file. But, in a
 - shared repository, the current user may not be able to change a file
 - owned by another user, so failure to set this mode is ignored.
 -}
freezeContent :: RawFilePath -> Annex ()
freezeContent file = unlessM crippledFileSystem $
	withShared go
  where
	go GroupShared = liftIO $ void $ tryIO $ modifyFileMode file $
		addModes [ownerReadMode, groupReadMode, ownerWriteMode, groupWriteMode]
	go AllShared = liftIO $ void $ tryIO $ modifyFileMode file $
		addModes (readModes ++ writeModes)
	go _ = liftIO $ modifyFileMode file $
		removeModes writeModes .
		addModes [ownerReadMode]

isContentWritePermOk :: RawFilePath -> Annex Bool
isContentWritePermOk file = ifM crippledFileSystem
	( return True
	, withShared go
	)
  where
	go GroupShared = want [ownerWriteMode, groupWriteMode]
	go AllShared = want writeModes
	go _ = return True
	want wantmode =
		liftIO (catchMaybeIO $ fileMode <$> R.getFileStatus file) >>= return . \case
			Nothing -> True
			Just havemode -> havemode == combineModes (havemode:wantmode)

{- Adjusts read mode of annexed file per core.sharedRepository setting. -}
chmodContent :: RawFilePath -> Annex ()
chmodContent file = unlessM crippledFileSystem $
	withShared go
  where
	go GroupShared = liftIO $ void $ tryIO $ modifyFileMode file $
		addModes [ownerReadMode, groupReadMode]
	go AllShared = liftIO $ void $ tryIO $ modifyFileMode file $
		addModes readModes
	go _ = liftIO $ modifyFileMode file $
		addModes [ownerReadMode]

{- Allows writing to an annexed file that freezeContent was called on
 - before. -}
thawContent :: RawFilePath -> Annex ()
thawContent file = thawPerms $ withShared go
  where
	go GroupShared = liftIO $ void $ tryIO $ groupWriteRead file
	go AllShared = liftIO $ void $ tryIO $ groupWriteRead file
	go _ = liftIO $ allowWrite file

{- Runs an action that thaws a file's permissions. This will probably
 - fail on a crippled filesystem. But, if file modes are supported on a
 - crippled filesystem, the file may be frozen, so try to thaw it. -}
thawPerms :: Annex () -> Annex ()
thawPerms a = ifM crippledFileSystem
	( void $ tryNonAsync a
	, a
	)

{- Blocks writing to the directory an annexed file is in, to prevent the
 - file accidentally being deleted. However, if core.sharedRepository
 - is set, this is not done, since the group must be allowed to delete the
 - file.
 -}
freezeContentDir :: RawFilePath -> Annex ()
freezeContentDir file = unlessM crippledFileSystem $
	withShared go
  where
	dir = parentDir file
	go GroupShared = liftIO $ void $ tryIO $ groupWriteRead dir
	go AllShared = liftIO $ void $ tryIO $ groupWriteRead dir
	go _ = liftIO $ preventWrite dir

thawContentDir :: RawFilePath -> Annex ()
thawContentDir file = 
	thawPerms $ liftIO $ allowWrite $ parentDir file

{- Makes the directory tree to store an annexed file's content,
 - with appropriate permissions on each level. -}
createContentDir :: RawFilePath -> Annex ()
createContentDir dest = do
	unlessM (liftIO $ R.doesPathExist dir) $
		createAnnexDirectory dir 
	-- might have already existed with restricted perms
	unlessM crippledFileSystem $
		liftIO $ allowWrite dir
  where
	dir = parentDir dest

{- Creates the content directory for a file if it doesn't already exist,
 - or thaws it if it does, then runs an action to modify the file, and
 - finally, freezes the content directory. -}
modifyContent :: RawFilePath -> Annex a -> Annex a
modifyContent f a = do
	createContentDir f -- also thaws it
	v <- tryNonAsync a
	freezeContentDir f
	either throwM return v

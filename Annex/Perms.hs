{- git-annex file permissions
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Perms (
	setAnnexFilePerm,
	setAnnexDirPerm,
	annexFileMode,
	createAnnexDirectory,
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
import Git.SharedRepository
import qualified Annex
import Config

withShared :: (SharedRepository -> Annex a) -> Annex a
withShared a = a =<< coreSharedRepository <$> Annex.getGitConfig

setAnnexFilePerm :: FilePath -> Annex ()
setAnnexFilePerm = setAnnexPerm False

setAnnexDirPerm :: FilePath -> Annex ()
setAnnexDirPerm = setAnnexPerm True

{- Sets appropriate file mode for a file or directory in the annex,
 - other than the content files and content directory. Normally,
 - use the default mode, but with core.sharedRepository set,
 - allow the group to write, etc. -}
setAnnexPerm :: Bool -> FilePath -> Annex ()
setAnnexPerm isdir file = unlessM crippledFileSystem $
	withShared $ liftIO . go
  where
	go GroupShared = void $ tryIO $ modifyFileMode file $ addModes $
		groupSharedModes ++
		if isdir then [ ownerExecuteMode, groupExecuteMode ] else []
	go AllShared = void $ tryIO $ modifyFileMode file $ addModes $
		readModes ++
		[ ownerWriteMode, groupWriteMode ] ++
		if isdir then executeModes else []
	go _ = noop

{- Gets the appropriate mode to use for creating a file in the annex
 - (other than content files, which are locked down more). -}
annexFileMode :: Annex FileMode
annexFileMode = withShared $ return . go
  where
	go GroupShared = sharedmode
	go AllShared = combineModes (sharedmode:readModes)
	go _ = stdFileMode
	sharedmode = combineModes groupSharedModes

{- Creates a directory inside the gitAnnexDir, including any parent
 - directories. Makes directories with appropriate permissions. -}
createAnnexDirectory :: FilePath -> Annex ()
createAnnexDirectory dir = walk dir [] =<< top
  where
	top = parentDir <$> fromRepo gitAnnexDir
	walk d below stop
		| d `equalFilePath` stop = done
		| otherwise = ifM (liftIO $ doesDirectoryExist d)
			( done
			, walk (parentDir d) (d:below) stop
			)
	  where
		done = forM_ below $ \p -> do
			liftIO $ createDirectoryIfMissing True p
			setAnnexDirPerm p

{- Normally, blocks writing to an annexed file, and modifies file
 - permissions to allow reading it.
 -
 - When core.sharedRepository is set, the write bits are not removed from
 - the file, but instead the appropriate group write bits are set. This is
 - necessary to let other users in the group lock the file. But, in a
 - shared repository, the current user may not be able to change a file
 - owned by another user, so failure to set this mode is ignored.
 -}
freezeContent :: FilePath -> Annex ()
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

isContentWritePermOk :: FilePath -> Annex Bool
isContentWritePermOk file = ifM crippledFileSystem
	( return True
	, withShared go
	)
  where
	go GroupShared = want [ownerWriteMode, groupWriteMode]
	go AllShared = want writeModes
	go _ = return True
	want wantmode = do
		mmode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus file
		return $ case mmode of
			Nothing -> True
			Just havemode -> havemode == combineModes (havemode:wantmode)

{- Adjusts read mode of annexed file per core.sharedRepository setting. -}
chmodContent :: FilePath -> Annex ()
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
thawContent :: FilePath -> Annex ()
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
freezeContentDir :: FilePath -> Annex ()
freezeContentDir file = unlessM crippledFileSystem $
	withShared go
  where
	dir = parentDir file
	go GroupShared = liftIO $ void $ tryIO $ groupWriteRead dir
	go AllShared = liftIO $ void $ tryIO $ groupWriteRead dir
	go _ = liftIO $ preventWrite dir

thawContentDir :: FilePath -> Annex ()
thawContentDir file = thawPerms $ liftIO $ allowWrite $ parentDir file

{- Makes the directory tree to store an annexed file's content,
 - with appropriate permissions on each level. -}
createContentDir :: FilePath -> Annex ()
createContentDir dest = do
	unlessM (liftIO $ doesDirectoryExist dir) $
		createAnnexDirectory dir 
	-- might have already existed with restricted perms
	unlessM crippledFileSystem $
		liftIO $ allowWrite dir
  where
	dir = parentDir dest

{- Creates the content directory for a file if it doesn't already exist,
 - or thaws it if it does, then runs an action to modify the file, and
 - finally, freezes the content directory. -}
modifyContent :: FilePath -> Annex a -> Annex a
modifyContent f a = do
	createContentDir f -- also thaws it
	v <- tryNonAsync a
	freezeContentDir f
	either throwM return v

{- git-annex file permissions
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Perms (
	setAnnexFilePerm,
	setAnnexDirPerm,
	annexFileMode,
	createAnnexDirectory,
	noUmask,
	createContentDir,
	freezeContentDir,
	thawContentDir,
	modifyContent,
) where

import Common.Annex
import Utility.FileMode
import Git.SharedRepository
import qualified Annex
import Annex.Exception
import Config

import System.Posix.Types

withShared :: (SharedRepository -> Annex a) -> Annex a
withShared a = maybe startup a =<< Annex.getState Annex.shared
  where
	startup = do
		shared <- fromRepo getSharedRepository
		Annex.changeState $ \s -> s { Annex.shared = Just shared }
		a shared

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
	go GroupShared = modifyFileMode file $ addModes $
		groupSharedModes ++
		if isdir then [ ownerExecuteMode, groupExecuteMode ] else []
	go AllShared = modifyFileMode file $ addModes $
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
createAnnexDirectory dir = traverse dir [] =<< top
  where
	top = parentDir <$> fromRepo gitAnnexDir
	traverse d below stop
		| d `equalFilePath` stop = done
		| otherwise = ifM (liftIO $ doesDirectoryExist d)
			( done
			, traverse (parentDir d) (d:below) stop
			)
	  where
		done = forM_ below $ \p -> do
			liftIO $ createDirectoryIfMissing True p
			setAnnexDirPerm p

{- Blocks writing to the directory an annexed file is in, to prevent the
 - file accidentially being deleted. However, if core.sharedRepository
 - is set, this is not done, since the group must be allowed to delete the
 - file.
 -}
freezeContentDir :: FilePath -> Annex ()
freezeContentDir file = unlessM crippledFileSystem $
	liftIO . go =<< fromRepo getSharedRepository
  where
	dir = parentDir file
	go GroupShared = groupWriteRead dir
	go AllShared = groupWriteRead dir
	go _ = preventWrite dir

thawContentDir :: FilePath -> Annex ()
thawContentDir file = unlessM crippledFileSystem $
	liftIO $ allowWrite $ parentDir file

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
	v <- tryAnnex a
	freezeContentDir f
	either throwAnnex return v

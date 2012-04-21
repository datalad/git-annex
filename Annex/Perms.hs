{- git-annex file permissions
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Perms (
	setAnnexPerm,
	annexFileMode,
	createAnnexDirectory,
	noUmask,
) where

import Common.Annex
import Utility.FileMode
import Git.SharedRepository

import System.Posix.Types

{- Sets appropriate file mode for a file or directory in the annex,
 - other than the content files and content directory. Normally,
 - use the default mode, but with core.sharedRepository set,
 - allow the group to write, etc. -}
setAnnexPerm :: FilePath -> Annex ()
setAnnexPerm file = liftIO . go =<< fromRepo getSharedRepository
	where
		go GroupShared = groupWriteRead file
		go AllShared = modifyFileMode file $ addModes $
			[ ownerWriteMode, groupWriteMode ] ++ readModes
		go _ = return ()

{- Gets the appropriate mode to use for creating a file in the annex
 - (other than content files, which are locked down more). -}
annexFileMode :: Annex FileMode
annexFileMode = go <$> fromRepo getSharedRepository
	where
		go GroupShared = sharedmode
		go AllShared = combineModes (sharedmode:readModes)
		go _ = stdFileMode
		sharedmode = combineModes
			[ ownerWriteMode, groupWriteMode
			, ownerReadMode, groupReadMode
			]

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
					liftIO $ createDirectory p
					setAnnexPerm p

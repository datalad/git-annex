{- file copying
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.CopyFile (
	copyFileExternal,
	copyCoW,
	createLinkOrCopy,
	CopyMetaData(..)
) where

import Common
import qualified BuildInfo
import qualified Utility.RawFilePath as R

data CopyMetaData 
	-- Copy timestamps when possible, but no other metadata, and
	-- when copying a symlink, makes a copy of its content.
	= CopyTimeStamps
	-- Copy all metadata when possible.
	| CopyAllMetaData
	deriving (Eq)

copyMetaDataParams :: CopyMetaData -> [CommandParam]
copyMetaDataParams meta = map snd $ filter fst
	[ (allmeta && BuildInfo.cp_a, Param "-a")
	, (allmeta && BuildInfo.cp_p && not BuildInfo.cp_a
		, Param "-p")
	, (not allmeta && BuildInfo.cp_preserve_timestamps
		, Param "--preserve=timestamps")
	-- cp -a may preserve xattrs that have special meaning,
	-- eg to NFS, and have even been observed to prevent later
	-- changing the permissions of the file. So prevent preserving
	-- xattrs.
	, (allmeta && BuildInfo.cp_a && BuildInfo.cp_no_preserve_xattr_supported
		, Param "--no-preserve=xattr")
	]
  where
	allmeta = meta == CopyAllMetaData

{- The cp command is used, because I hate reinventing the wheel,
 - and because this allows easy access to features like cp --reflink
 - and preserving metadata. -}
copyFileExternal :: CopyMetaData -> FilePath -> FilePath -> IO Bool
copyFileExternal meta src dest = do
	-- Delete any existing dest file because an unwritable file
	-- would prevent cp from working.
	void $ tryIO $ removeFile dest
	boolSystem "cp" $ params ++ [File src, File dest]
  where
	params
		| BuildInfo.cp_reflink_supported =
			Param "--reflink=auto" : copyMetaDataParams meta
		| otherwise = copyMetaDataParams meta

{- When a filesystem supports CoW (and cp does), uses it to make
 - an efficient copy of a file. Otherwise, returns False.
 -
 - The dest file must not exist yet, or it will fail to make a CoW copy,
 - and will return False.
 -}
copyCoW :: CopyMetaData -> FilePath -> FilePath -> IO Bool
copyCoW meta src dest
	| BuildInfo.cp_reflink_supported = do
		-- When CoW is not supported, cp will complain to stderr,
		-- so have to discard its stderr.
		ok <- catchBoolIO $ withNullHandle $ \nullh ->
			let p = (proc "cp" $ toCommand $ params ++ [File src, File dest])
				{ std_out = UseHandle nullh
				, std_err = UseHandle nullh
				}
			in withCreateProcess p $ \_ _ _ -> checkSuccessProcess
		-- When CoW is not supported, cp creates the destination
		-- file but leaves it empty.
		unless ok $
			void $ tryIO $ removeFile dest
		return ok
	| otherwise = return False
  where
 	-- Note that in coreutils 9.0, cp uses CoW by default,
	-- without needing an option. This s only needed to support 
	-- older versions.
	params = Param "--reflink=always" : copyMetaDataParams meta

{- Create a hard link if the filesystem allows it, and fall back to copying
 - the file. -}
createLinkOrCopy :: RawFilePath -> RawFilePath -> IO Bool
createLinkOrCopy src dest = go `catchIO` const fallback
  where
	go = do
		R.createLink src dest
		return True
	fallback = copyFileExternal CopyAllMetaData (fromRawFilePath src) (fromRawFilePath dest)

{- file copying
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
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
 - an efficient copy of a file. Otherwise, returns False. -}
copyCoW :: CopyMetaData -> FilePath -> FilePath -> IO Bool
copyCoW meta src dest
	| BuildInfo.cp_reflink_supported = do
		void $ tryIO $ removeFile dest
		-- When CoW is not supported, cp will complain to stderr,
		-- so have to discard its stderr.
		ok <- catchBoolIO $ do
			withQuietOutput createProcessSuccess $
				proc "cp" $ toCommand $
					params ++ [File src, File dest]
			return True
		-- When CoW is not supported, cp creates the destination
		-- file but leaves it empty.
		unless ok $
			void $ tryIO $ removeFile dest
		return ok
	| otherwise = return False
  where
	params = Param "--reflink=always" : copyMetaDataParams meta

{- Create a hard link if the filesystem allows it, and fall back to copying
 - the file. -}
createLinkOrCopy :: FilePath -> FilePath -> IO Bool
createLinkOrCopy src dest = go `catchIO` const fallback
  where
	go = do
		createLink src dest
		return True
	fallback = copyFileExternal CopyAllMetaData src dest

{- filenames (not paths) used in views
 -
 - Copyright 2014-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Annex.View.ViewedFile (
	ViewedFile,
	MkViewedFile,
	viewedFileFromReference,
	viewedFileFromReference',
	viewedFileReuse,
	dirFromViewedFile,
	prop_viewedFile_roundtrips,
) where

import Annex.Common
import Utility.QuickCheck
import Backend.Utilities (maxExtensions)
import qualified Utility.OsString as OS

import qualified Data.ByteString as S

type FileName = String
type ViewedFile = FileName

type MkViewedFile = OsPath -> ViewedFile

{- Converts a filepath used in a reference branch to the
 - filename that will be used in the view.
 -
 - No two filepaths from the same branch should yield the same result,
 - so all directory structure needs to be included in the output filename
 - in some way.
 -
 - So, from dir/subdir/file.foo, generate file_%dir%subdir%.foo
 -}
viewedFileFromReference :: GitConfig -> MkViewedFile
viewedFileFromReference g = viewedFileFromReference'
	(annexMaxExtensionLength g)
	(annexMaxExtensions g)

viewedFileFromReference' :: Maybe Int -> Maybe Int -> MkViewedFile
viewedFileFromReference' maxextlen maxextensions f = concat $
	[ escape (fromOsPath base')
	, if null dirs
		then ""
		else "_%" ++ intercalate "%" (map (escape . fromOsPath) dirs) ++ "%"
	, escape $ fromRawFilePath $ S.concat extensions'
	]
  where
	(path, basefile) = splitFileName f
	dirs = filter (/= literalOsPath ".") $
		map dropTrailingPathSeparator (splitPath path)
	(base, extensions) = case maxextlen of
		Nothing -> splitShortExtensions basefile'
		Just n -> splitShortExtensions' (n+1) basefile'
	{- Limit number of extensions. -}
	maxextensions' = fromMaybe maxExtensions maxextensions
	(base', extensions')
		| length extensions <= maxextensions' = (base, extensions)
		| otherwise = 
			let (es,more) = splitAt maxextensions' (reverse extensions)
			in (base <> toOsPath (mconcat (reverse more)), reverse es)
	{- On Windows, if the filename looked like "dir/c:foo" then
	 - basefile would look like it contains a drive letter, which will
	 - not work. There cannot really be a filename like that, probably,
	 - but it prevents the test suite failing. -}
	(_basedrive, basefile') = splitDrive basefile

	{- To avoid collisions with filenames or directories that contain
	 - '%', and to allow the original directories to be extracted
	 - from the ViewedFile, '%' is escaped. )
	 -}
	escape :: String -> String
	escape = replace "%" (escchar:'%':[]) . replace [escchar] [escchar, escchar]

escchar :: Char
#ifndef mingw32_HOST_OS
escchar = '\\'
#else
-- \ is path separator on Windows, so instead use !
escchar = '!'
#endif

{- For use when operating already within a view, so whatever filepath
 - is present in the work tree is already a ViewedFile. -}
viewedFileReuse :: MkViewedFile
viewedFileReuse = fromOsPath . takeFileName

{- Extracts from a ViewedFile the directory where the file is located on
 - in the reference branch. -}
dirFromViewedFile :: ViewedFile -> FilePath
dirFromViewedFile = fromOsPath . joinPath . map toOsPath . drop 1 . sep [] ""
  where
	sep l _ [] = reverse l
	sep l curr (c:cs)
		| c == '%' = sep (reverse curr:l) "" cs
		| c == escchar = case cs of
			(c':cs') -> sep l (c':curr) cs'
			[] -> sep l curr cs
		| otherwise = sep l (c:curr) cs

prop_viewedFile_roundtrips :: TestableFilePath -> Bool
prop_viewedFile_roundtrips tf
	-- Relative filenames wanted, not directories.
	| OS.any isPathSeparator (toOsPath (end f ++ beginning f)) = True
	| isAbsolute (toOsPath f) || isDrive (toOsPath f) = True
	| otherwise = fromOsPath dir == dirFromViewedFile 
		(viewedFileFromReference' Nothing Nothing (toOsPath f))
  where
	f = fromTestableFilePath tf
	dir = joinPath $ beginning $ splitDirectories (toOsPath f)

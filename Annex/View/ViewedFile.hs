{- filenames (not paths) used in views
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.View.ViewedFile (
	ViewedFile,
	MkViewedFile,
	viewedFileFromReference,
	viewedFileReuse,
	dirFromViewedFile,
	prop_viewedFile_roundtrips,
) where

import Annex.Common

type FileName = String
type ViewedFile = FileName

type MkViewedFile = FilePath -> ViewedFile

{- Converts a filepath used in a reference branch to the
 - filename that will be used in the view.
 -
 - No two filepaths from the same branch should yeild the same result,
 - so all directory structure needs to be included in the output filename
 - in some way.
 -
 - So, from dir/subdir/file.foo, generate file_%dir%subdir%.foo
 -}
viewedFileFromReference :: MkViewedFile
viewedFileFromReference f = concat
	[ escape base
	, if null dirs then "" else "_%" ++ intercalate "%" (map escape dirs) ++ "%"
	, escape $ concat extensions
	]
  where
	(path, basefile) = splitFileName f
	dirs = filter (/= ".") $ map dropTrailingPathSeparator (splitPath path)
	(base, extensions) = splitShortExtensions basefile

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
viewedFileReuse = takeFileName

{- Extracts from a ViewedFile the directory where the file is located on
 - in the reference branch. -}
dirFromViewedFile :: ViewedFile -> FilePath
dirFromViewedFile = joinPath . drop 1 . sep [] ""
  where
	sep l _ [] = reverse l
	sep l curr (c:cs)
		| c == '%' = sep (reverse curr:l) "" cs
		| c == escchar = case cs of
			(c':cs') -> sep l (c':curr) cs'
			[] -> sep l curr cs
		| otherwise = sep l (c:curr) cs

prop_viewedFile_roundtrips :: FilePath -> Bool
prop_viewedFile_roundtrips f
	-- Relative filenames wanted, not directories.
	| any (isPathSeparator) (end f ++ beginning f) = True
	| isAbsolute f = True
	| otherwise = dir == dirFromViewedFile (viewedFileFromReference f)
  where
	dir = joinPath $ beginning $ splitDirectories f

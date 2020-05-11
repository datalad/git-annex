{- handling untrusted filepaths
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.UntrustedFilePath where

import Data.Char
import System.FilePath

{- Given a string that we'd like to use as the basis for FilePath, but that
 - was provided by a third party and is not to be trusted, returns the closest
 - sane FilePath.
 -
 - All spaces and punctuation and other wacky stuff are replaced
 - with '_', except for '.' and '-'
 -
 - "../" becomes ".._", which is safe.
 - "/foo" becomes "_foo", which is safe.
 - "c:foo" becomes "c_foo", which is safe even on windows.
 - 
 - Leading '.' and '-' are also replaced with '_', so
 - so no dotfiles that might control a program are inadvertently created,
 - and to avoid filenames being treated as options to commands the user
 - might run.
 -
 - Also there's an off chance the string might be empty, so to avoid
 - needing to handle such an invalid filepath, return a dummy "file" in
 - that case.
 -}
sanitizeFilePath :: String -> FilePath
sanitizeFilePath [] = "file"
sanitizeFilePath f = leading (map sanitize f)
  where
	sanitize c
		| c == '.' || c == '-' = c
		| isSpace c || isPunctuation c || isSymbol c || isControl c || c == '/' = '_'
		| otherwise = c

	leading ('.':s) = '_':s
	leading ('-':s) = '_':s
	leading s = s

escapeSequenceInFilePath :: FilePath -> Bool
escapeSequenceInFilePath f = '\ESC' `elem` f

{- ../ is a path traversal, no matter where it appears.
 -
 - An absolute path is, of course.
 -}
pathTraversalInFilePath :: FilePath -> Bool
pathTraversalInFilePath f
	| isAbsolute f = True
	| any (== "..") (splitPath f) = True
	-- On windows, C:foo with no directory is not considered absolute
	| hasDrive f = True 
	| otherwise = False

gitDirectoryInFilePath :: FilePath -> Bool
gitDirectoryInFilePath = any (== ".git")
	. map dropTrailingPathSeparator
	. splitPath 

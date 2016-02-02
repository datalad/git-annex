{- git check-attr interface
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CheckAttr where

import Common
import Git
import Git.Command
import qualified Git.Version
import qualified Utility.CoProcess as CoProcess

import System.IO.Error

type CheckAttrHandle = (CoProcess.CoProcessHandle, [Attr], Bool, String)

type Attr = String

{- Starts git check-attr running to look up the specified gitattributes
 - values and returns a handle.  -}
checkAttrStart :: [Attr] -> Repo -> IO CheckAttrHandle
checkAttrStart attrs repo = do
	currdir <- getCurrentDirectory
	h <- CoProcess.rawMode =<< gitCoProcessStart True params repo
	oldgit <- Git.Version.older "1.7.7"
	return (h, attrs, oldgit, currdir)
  where
	params =
		[ Param "check-attr" 
		, Param "-z"
		, Param "--stdin"
		] ++ map Param attrs ++
		[ Param "--" ]

checkAttrStop :: CheckAttrHandle -> IO ()
checkAttrStop (h, _, _, _) = CoProcess.stop h

{- Gets an attribute of a file. -}
checkAttr :: CheckAttrHandle -> Attr -> FilePath -> IO String
checkAttr (h, attrs, oldgit, currdir) want file = do
	pairs <- CoProcess.query h send (receive "")
	let vals = map snd $ filter (\(attr, _) -> attr == want) pairs
	case vals of
		[v] -> return v
		_ -> error $ "unable to determine " ++ want ++ " attribute of " ++ file
  where
	send to = hPutStr to $ file' ++ "\0"
	receive c from = do
		s <- hGetSomeString from 1024
		if null s
			then eofError
			else do
				let v = c ++ s
				maybe (receive v from) return (parse v)
	eofError = ioError $ mkIOError userErrorType "git check-attr EOF" Nothing Nothing
	parse s
		-- new null separated output
		| '\0' `elem` s = if "\0" `isSuffixOf` s
			then
				let bits = segment (== '\0') s
				in if length bits == (numattrs * 3) + 1
					then Just $ getattrvalues bits []
					else Nothing -- more attributes to come
			else Nothing -- output incomplete
		-- old one line per value output
		| otherwise = if "\n" `isSuffixOf` s
			then
				let ls = lines s
				in if length ls == numattrs
					then Just $ map (\(attr, val) -> (attr, oldattrvalue attr val))
						(zip attrs ls)
					else Nothing -- more attributes to come
			else Nothing -- line incomplete
	numattrs = length attrs

	{- Before git 1.7.7, git check-attr worked best with
	 - absolute filenames; using them worked around some bugs
	 - with relative filenames.
	 - 
	 - With newer git, git check-attr chokes on some absolute
	 - filenames, and the bugs that necessitated them were fixed,
	 - so use relative filenames. -}
	file'
		| oldgit = absPathFrom currdir file
		| otherwise = relPathDirToFileAbs currdir $ absPathFrom currdir file
	oldattrvalue attr l = end bits !! 0
	  where
		bits = split sep l
		sep = ": " ++ attr ++ ": "
	getattrvalues (_filename:attr:val:rest) c = getattrvalues rest ((attr,val):c)
	getattrvalues _ c = c

unspecifiedAttr :: String
unspecifiedAttr = "!"

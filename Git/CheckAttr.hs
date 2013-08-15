{- git check-attr interface
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CheckAttr where

import Common
import Git
import Git.Command
import qualified Git.BuildVersion
import qualified Utility.CoProcess as CoProcess

type CheckAttrHandle = (CoProcess.CoProcessHandle, [Attr], String)

type Attr = String

{- Starts git check-attr running to look up the specified gitattributes
 - values and returns a handle.  -}
checkAttrStart :: [Attr] -> Repo -> IO CheckAttrHandle
checkAttrStart attrs repo = do
	cwd <- getCurrentDirectory
	h <- CoProcess.rawMode =<< gitCoProcessStart True params repo
	return (h, attrs, cwd)
  where
	params =
		[ Param "check-attr" 
		, Params "-z --stdin"
		] ++ map Param attrs ++
		[ Param "--" ]

checkAttrStop :: CheckAttrHandle -> IO ()
checkAttrStop (h, _, _) = CoProcess.stop h

{- Gets an attribute of a file. -}
checkAttr :: CheckAttrHandle -> Attr -> FilePath -> IO String
checkAttr (h, attrs, cwd) want file = do
	pairs <- CoProcess.query h send receive
	let vals = map snd $ filter (\(attr, _) -> attr == want) pairs
	case vals of
		[v] -> return v
		_ -> error $ "unable to determine " ++ want ++ " attribute of " ++ file
  where
	send to = hPutStr to $ file' ++ "\0"
	receive from = forM attrs $ \attr -> do
		l <- hGetLine from
		return (attr, attrvalue attr l)
	{- Before git 1.7.7, git check-attr worked best with
	 - absolute filenames; using them worked around some bugs
	 - with relative filenames.
	 - 
	 - With newer git, git check-attr chokes on some absolute
	 - filenames, and the bugs that necessitated them were fixed,
	 - so use relative filenames. -}
	oldgit = Git.BuildVersion.older "1.7.7"
	file'
		| oldgit = absPathFrom cwd file
		| otherwise = relPathDirToFile cwd $ absPathFrom cwd file
	attrvalue attr l = end bits !! 0
	  where
		bits = split sep l
		sep = ": " ++ attr ++ ": "

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
import qualified Git.Version

type CheckAttrHandle = (PipeHandle, Handle, Handle, [Attr], String)

type Attr = String

{- Starts git check-attr running to look up the specified gitattributes
 - values and return a handle.  -}
checkAttrStart :: [Attr] -> Repo -> IO CheckAttrHandle
checkAttrStart attrs repo = do
	cwd <- getCurrentDirectory
	(pid, from, to) <- hPipeBoth "git" $ toCommand $
		gitCommandLine params repo
	return (pid, from, to, attrs, cwd)
	where
		params =
			[ Param "check-attr" ]
			++ map Param attrs ++
			[ Params "-z --stdin" ]

{- Stops git check-attr. -}
checkAttrStop :: CheckAttrHandle -> IO ()
checkAttrStop (pid, from, to, _, _) = do
	hClose to
	hClose from
	forceSuccess pid

{- Gets an attribute of a file. -}
checkAttr :: CheckAttrHandle -> Attr -> FilePath -> IO String
checkAttr (_, from, to, attrs, cwd) want file = do
	hPutStr to $ file' ++ "\0"
	hFlush to
	pairs <- forM attrs $ \attr -> do
		l <- hGetLine from
		return (attr, attrvalue attr l)
	let vals = map snd $ filter (\(attr, _) -> attr == want) pairs
	case vals of
		[v] -> return v
		_ -> error $ "unable to determine " ++ want ++ " attribute of " ++ file
	where
		{- Before git 1.7.7, git check-attr worked best with
		 - absolute filenames; using them worked around some bugs
		 - with relative filenames.
		 - 
		 - With newer git, git check-attr chokes on some absolute
		 - filenames, and the bugs that necessitated them were fixed,
		 - so use relative filenames. -}
		oldgit = Git.Version.older "1.7.7"
		file'
			| oldgit = absPathFrom cwd file
			| otherwise = relPathDirToFile cwd $ absPathFrom cwd file
		attrvalue attr l = end bits !! 0
			where
				bits = split sep l
				sep = ": " ++ attr ++ ": "

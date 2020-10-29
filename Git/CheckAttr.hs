{- git check-attr interface
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.CheckAttr where

import Common
import Git
import Git.Command
import qualified Utility.CoProcess as CoProcess
import qualified Utility.RawFilePath as R

import System.IO.Error
import qualified Data.ByteString as B

type CheckAttrHandle = (CoProcess.CoProcessHandle, [Attr], RawFilePath)

type Attr = String

{- Starts git check-attr running to look up the specified gitattributes
 - values and returns a handle.  -}
checkAttrStart :: [Attr] -> Repo -> IO CheckAttrHandle
checkAttrStart attrs repo = do
	currdir <- R.getCurrentDirectory
	h <- gitCoProcessStart True params repo
	return (h, attrs, currdir)
  where
	params =
		[ Param "check-attr" 
		, Param "-z"
		, Param "--stdin"
		] ++ map Param attrs ++
		[ Param "--" ]

checkAttrStop :: CheckAttrHandle -> IO ()
checkAttrStop (h, _, _) = CoProcess.stop h

{- Gets an attribute of a file. When the attribute is not specified,
 - returns "" -}
checkAttr :: CheckAttrHandle -> Attr -> RawFilePath -> IO String
checkAttr (h, attrs, currdir) want file = do
	pairs <- CoProcess.query h send (receive "")
	let vals = map snd $ filter (\(attr, _) -> attr == want) pairs
	case vals of
		["unspecified"] -> return ""
		[v] -> return v
		_ -> error $ "unable to determine " ++ want ++ " attribute of " ++ fromRawFilePath file
  where
	send to = B.hPutStr to $ file' `B.snoc` 0
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

	{- git check-attr chokes on some absolute filenames,
	 - so make sure the filename is relative. -}
	file' = relPathDirToFileAbs currdir $ absPathFrom currdir file
	oldattrvalue attr l = end bits !! 0
	  where
		bits = split sep l
		sep = ": " ++ attr ++ ": "
	getattrvalues (_filename:attr:val:rest) c = getattrvalues rest ((attr,val):c)
	getattrvalues _ c = c

{- User may enter this to override a previous attr setting, when they wish
 - to not specify an attr for some files. -}
unspecifiedAttr :: String
unspecifiedAttr = "!"

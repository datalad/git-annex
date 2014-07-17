{- git-annex backend utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.Utilities where

import Data.Hash.MD5

import Common.Annex

{- Generates a keyName from an input string. Takes care of sanitizing it.
 - If it's not too long, the full string is used as the keyName.
 - Otherwise, it's truncated at half the filename length limit, and its
 - md5 is prepended to ensure a unique key. -}
genKeyName :: String -> Annex String
genKeyName s = do
	limit <- liftIO . fileNameLengthLimit =<< fromRepo gitAnnexDir
	let s' = preSanitizeKeyName s
	let truncs = truncateFilePath (limit `div` 2) s'
	return $ if s' == truncs
		then s'
		else truncs ++ "-" ++ md5s (Str s)

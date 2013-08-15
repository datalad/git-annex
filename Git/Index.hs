{- git index file stuff
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Index where

import Utility.Env

{- Forces git to use the specified index file.
 -
 - Returns an action that will reset back to the default
 - index file.
 -
 - Warning: Not thread safe.
 -}
override :: FilePath -> IO (IO ())
override index = do
	res <- getEnv var
	setEnv var index True
	return $ reset res
  where
	var = "GIT_INDEX_FILE"
	reset (Just v) = setEnv var v True
	reset _ = unsetEnv var

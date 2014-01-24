{- git hooks
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Hook where

import Common
import Git
import Utility.Tmp

data Hook = Hook
	{ hookName :: FilePath
	, hookScript :: String
	}

hookFile :: Hook -> Repo -> FilePath
hookFile h r = localGitDir r </> "hooks" </> hookName h

{- Writes a hook. Returns False if the hook already exists with a different
 - content. -}
hookWrite :: Hook -> Repo -> IO Bool
hookWrite h r = do
	let f = hookFile h r
	ifM (doesFileExist f)
		( expectedContent h r
		, do
			viaTmp writeFile f (hookScript h)
			p <- getPermissions f
			setPermissions f $ p {executable = True}
			return True
		)

{- Removes a hook. Returns False if the hook contained something else, and
 - could not be removed. -}
hookUnWrite :: Hook -> Repo -> IO Bool
hookUnWrite h r = do
	let f = hookFile h r
	ifM (doesFileExist f)
		( ifM (expectedContent h r)
			( do
				removeFile f
				return True
			, return False
			)
		, return True
		)

expectedContent :: Hook -> Repo -> IO Bool
expectedContent h r = do
	content <- readFile $ hookFile h r
	return $ content == hookScript h

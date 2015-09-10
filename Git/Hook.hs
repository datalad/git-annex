{- git hooks
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Hook where

import Common
import Git
import Utility.Tmp
#ifndef mingw32_HOST_OS
import Utility.FileMode
#endif

data Hook = Hook
	{ hookName :: FilePath
	, hookScript :: String
	}
	deriving (Ord)

instance Eq Hook where
	a == b = hookName a == hookName b

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

hookExists :: Hook -> Repo -> IO Bool
hookExists h r = do
	let f = hookFile h r
	catchBoolIO $
#ifndef mingw32_HOST_OS
		isExecutable . fileMode <$> getFileStatus f
#else
		doesFileExist f
#endif

runHook :: Hook -> Repo -> IO Bool
runHook h r = do
	let f = hookFile h r
	(c, ps) <- findcmd f
	boolSystem c ps
  where
#ifndef mingw32_HOST_OS
	findcmd = defcmd
#else
	{- Like git for windows, parse the first line of the hook file,
	 - look for "#!", and dispatch the interpreter on the file. -}
	findcmd f = do
		l <- headMaybe . lines <$> catchDefaultIO "" (readFile f)
		case l of
			Just ('#':'!':rest) -> case words rest of
				[] -> defcmd f
				(c:ps) -> do
					let ps' = map Param (ps ++ [f])
					ok <- inPath c
					return (if ok then c else takeFileName c, ps')
			_ -> defcmd f
#endif
	defcmd f = return (f, [])

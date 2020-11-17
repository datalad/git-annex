{- git hooks
 -
 - Copyright 2013-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Hook where

import Common
import Git
import Utility.Tmp
import Utility.Shell
#ifndef mingw32_HOST_OS
import Utility.FileMode
#endif

data Hook = Hook
	{ hookName :: FilePath
	, hookScript :: String
	, hookOldScripts :: [String]
	}
	deriving (Ord)

instance Eq Hook where
	a == b = hookName a == hookName b

hookFile :: Hook -> Repo -> FilePath
hookFile h r = fromRawFilePath (localGitDir r) </> "hooks" </> hookName h

{- Writes a hook. Returns False if the hook already exists with a different
 - content. Upgrades old scripts.
 -
 - This can install hooks on both filesystem like FAT that do not support
 - execute bits, and on Windows.
 -
 - If the filesystem does not support execute bits, it's typically mounted
 - such that all files have the execute bit set. So just write the hook
 - and ignore failure to make it executable.
 -
 - On Windows, git will run hooks that are not executable. The hook
 - is run with a bundled bash, so should start with #!/bin/sh
 -}
hookWrite :: Hook -> Repo -> IO Bool
hookWrite h r = ifM (doesFileExist f)
	( expectedContent h r >>= \case
		UnexpectedContent -> return False
		ExpectedContent -> return True
		OldExpectedContent -> go
	, go
	)
  where
	f = hookFile h r
	go = do
		viaTmp writeFile f (hookScript h)
		void $ tryIO $ modifyFileMode
			(toRawFilePath f)
			(addModes executeModes)
		return True

{- Removes a hook. Returns False if the hook contained something else, and
 - could not be removed. -}
hookUnWrite :: Hook -> Repo -> IO Bool
hookUnWrite h r = ifM (doesFileExist f)
	( expectedContent h r >>= \case
		UnexpectedContent -> return False
		_ -> do
			removeFile f
			return True
	, return True
	)
  where
	f = hookFile h r

data ExpectedContent = UnexpectedContent | ExpectedContent | OldExpectedContent

expectedContent :: Hook -> Repo -> IO ExpectedContent
expectedContent h r = do
	content <- readFile $ hookFile h r
	return $ if content == hookScript h
		then ExpectedContent
		else if any (content ==) (hookOldScripts h)
			then OldExpectedContent
			else UnexpectedContent

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
	(c, ps) <- findShellCommand f
	boolSystem c ps

{- git-annex command
 -
 - Copyright 2013-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.LookupKey where

import Command
import Annex.CatFile
import qualified Git.LsFiles
import Utility.Terminal
import Utility.SafeOutput

cmd :: Command
cmd = notBareRepo $ noCommit $ noMessages $
	command "lookupkey" SectionPlumbing 
		"looks up key used for file"
		(paramRepeating paramFile)
		(batchable run (pure ()))

run :: () -> SeekInput -> String -> Annex Bool
run _ _ file = seekSingleGitFile file >>= \case
	Nothing -> return False
	Just file' -> catKeyFile file' >>= \case
		Just k  -> do
			IsTerminal isterminal <- liftIO $ checkIsTerminal stdout
			let sk = serializeKey k
			liftIO $ putStrLn $ if isterminal then safeOutput sk else sk
			return True
		Nothing -> return False

-- To support absolute filenames, pass through git ls-files.
-- But, this plumbing command does not recurse through directories.
seekSingleGitFile :: FilePath -> Annex (Maybe RawFilePath)
seekSingleGitFile file = do
	(l, cleanup) <- inRepo (Git.LsFiles.inRepo [] [toRawFilePath file])
	r <- case l of
		(f:[]) | takeFileName (fromRawFilePath f) == takeFileName file ->
			return (Just f)
		_ -> return Nothing
	void $ liftIO cleanup
	return r

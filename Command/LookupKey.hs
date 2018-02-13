{- git-annex command
 -
 - Copyright 2013-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.LookupKey where

import Command
import Annex.CatFile
import qualified Git.LsFiles

cmd :: Command
cmd = notBareRepo $ noCommit $ noMessages $
	command "lookupkey" SectionPlumbing 
		"looks up key used for file"
		(paramRepeating paramFile)
		(batchable run (pure ()))

run :: () -> String -> Annex Bool
run _ file = seekSingleGitFile file >>= \case
	Nothing -> return False
	Just file' -> catKeyFile file' >>= \case
		Just k  -> do
			liftIO $ putStrLn $ key2file k
			return True
		Nothing -> return False

-- To support absolute filenames, pass through git ls-files.
-- But, this plumbing command does not recurse through directories.
seekSingleGitFile :: FilePath -> Annex (Maybe FilePath)
seekSingleGitFile file = do
	(l, cleanup) <- inRepo (Git.LsFiles.inRepo [file])
	r <- case l of
		(f:[]) | takeFileName f == takeFileName file -> return (Just f)
		_ -> return Nothing
	void $ liftIO cleanup
	return r

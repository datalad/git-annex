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
import Git.Types
import Utility.Terminal
import Utility.SafeOutput

cmd :: Command
cmd = noCommit $ noMessages $
	command "lookupkey" SectionPlumbing 
		"looks up key used for file"
		(paramRepeating paramFile)
		(batchable run optParser)

data LookupKeyOptions = LookupKeyOptions
	{ refOption :: Bool
	}

optParser :: Parser LookupKeyOptions
optParser = LookupKeyOptions
	<$> switch
		( long "ref"
		<> help "look up key used by git ref to file"
		)

run :: LookupKeyOptions -> SeekInput -> String -> Annex Bool
run o _ file
	| refOption o = catKey (Ref (toRawFilePath file)) >>= display
	| otherwise = do
		checkNotBareRepo
		seekSingleGitFile file >>= \case
			Nothing -> return False
			Just file' -> catKeyFile file' >>= display

display :: Maybe Key -> Annex Bool
display (Just k) = do
	IsTerminal isterminal <- liftIO $ checkIsTerminal stdout
	let sk = serializeKey k
	liftIO $ putStrLn $ if isterminal then safeOutput sk else sk
	return True
display Nothing = return False

-- To support absolute filenames, pass through git ls-files.
-- But, this plumbing command does not recurse through directories.
seekSingleGitFile :: FilePath -> Annex (Maybe RawFilePath)
seekSingleGitFile file
	| isRelative file = return (Just (toRawFilePath file))
	| otherwise = do
		(l, cleanup) <- inRepo (Git.LsFiles.inRepo [] [toRawFilePath file])
		r <- case l of
			(f:[]) | takeFileName (fromRawFilePath f) == takeFileName file ->
				return (Just f)
			_ -> return Nothing
		void $ liftIO cleanup
		return r

{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Indirect where

import Common.Annex
import Command
import qualified Git
import qualified Git.Command
import qualified Git.LsFiles
import Config
import Annex.Direct
import Annex.Content
import Annex.CatFile

def :: [Command]
def = [notBareRepo $ command "indirect" paramNothing seek
	"switch repository to indirect mode"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = ifM isDirect ( next perform, stop )

perform :: CommandPerform
perform = do
	showStart "commit" ""
	whenM (stageDirect) $ do
		showOutput
		void $ inRepo $ Git.Command.runBool "commit"
			[Param "-m", Param "commit before switching to indirect mode"]
	showEndOk

	-- Note that we set indirect mode early, so that we can use
	-- moveAnnex in indirect mode.
	setDirect False

	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.stagedDetails [top]
	forM_ l go
	void $ liftIO clean
	next cleanup
  where
	{- Walk tree from top and move all present direct mode files into
	 - the annex, replacing with symlinks. Also delete direct mode
	 - caches and mappings. -}
	go (_, Nothing) = noop
	go (f, Just sha) = do
		r <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus f
		case r of
			Just s
				| isSymbolicLink s -> void $ flip whenAnnexed f $
					\_ (k, _) -> do
						cleandirect k
						return Nothing
				| otherwise -> 
					maybe noop (fromdirect f)
						=<< catKey sha
			_ -> noop

	fromdirect f k = do
		showStart "indirect" f
		cleandirect k -- clean before content directory gets frozen
		whenM (liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f) $ do
			moveAnnex k f
			l <- calcGitLink f k
			liftIO $ createSymbolicLink l f
		showEndOk

	cleandirect k = do
		liftIO . nukeFile =<< inRepo (gitAnnexCache k)
		liftIO . nukeFile =<< inRepo (gitAnnexMapping k)

cleanup :: CommandCleanup
cleanup = do
	showStart "indirect" ""
	showEndOk
	return True

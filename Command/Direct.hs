{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Direct where

import Common.Annex
import Command
import qualified Git
import qualified Git.Command
import qualified Git.LsFiles
import Config
import Annex.Content
import Annex.Content.Direct

def :: [Command]
def = [command "direct" paramNothing seek "switch repository to direct mode"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = notBareRepo $
	ifM isDirect
		( stop , next perform )

perform :: CommandPerform
perform = do
	showStart "commit" ""
	showOutput
	_ <- inRepo $ Git.Command.runBool "commit"
		[Param "-a", Param "-m", Param "commit before switching to direct mode"]
	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.inRepo [top]
	forM_ l go
	void $ liftIO clean
	next cleanup
  where
	{- Walk tree from top and move all present objects to the
	 - files that link to them, while updating direct mode mappings. -}
	go = whenAnnexed $ \f (k, _) -> do
		loc <- inRepo $ gitAnnexLocation k
		createContentDir loc -- thaws directory too
		locs <- filter (/= f) <$> addAssociatedFile k f
		case locs of
			[] -> whenM (liftIO $ doesFileExist loc) $ do
				{- Move content from annex to direct file. -}
				showStart "direct" f
				updateCache k loc
				thawContent loc
				liftIO $ replaceFile f $ moveFile loc
				showEndOk
			(loc':_) -> do
				{- Another direct file has the content, so
				 - hard link to it. -}
				showStart "direct" f
				liftIO $ replaceFile f $ createLink loc'
				showEndOk
		return Nothing

cleanup :: CommandCleanup
cleanup = do
	setDirect True
	return True

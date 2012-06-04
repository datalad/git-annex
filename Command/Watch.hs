{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Watch where

import CmdLine
import Common.Annex
import Command
import Utility.Inotify
import qualified Annex
import qualified Command.Add as Add
import qualified Git.Command
import qualified Annex.Queue
import qualified Backend
import Annex.Content

import Control.Exception as E
import System.INotify

def :: [Command]
def = [command "watch" paramPaths seek "watch for changes"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = notBareRepo $ do
	showStart "watch" "."
	showAction "scanning"
	state <- Annex.getState id
	next $ next $ liftIO $ withINotify $ \i -> do
		let hook a = Just $ run state a
		watchDir i "." (not . gitdir)
			(hook onAdd) (hook onAddSymlink)
			(hook onDel) (hook onDelDir)
		putStrLn "(started)"
		waitForTermination
		return True
	where
		gitdir dir = takeFileName dir /= ".git"

{- Inotify events are run in separate threads, and so each is a
 - self-contained Annex monad. Exceptions by the handlers are ignored,
 - otherwise a whole watcher thread could be crashed. -}
run :: Annex.AnnexState -> (FilePath -> Annex a) -> FilePath -> IO ()
run startstate a f = do
	r <- E.try go :: IO (Either E.SomeException ())
	case r of
		Left e -> putStrLn (show e)
		_ -> return ()
	where
		go = Annex.eval startstate $ do
			_ <- a f
			_ <- shutdown True
			return ()

{- Adding a file is the same as git-annex add.
 - The git queue is immediately flushed, so the file is added to git
 - now, rather than later (when it may have been already moved or deleted!) -}
onAdd :: FilePath -> Annex ()
onAdd file = doQuietSideAction $ do
	void $ doCommand $ do
		showStart "add" file
		next $ Add.perform file
	Annex.Queue.flush

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: FilePath -> Annex ()
onAddSymlink file = go =<< Backend.lookupFile file
	where
		go Nothing = addlink
		go (Just (key, _)) = do
			link <- calcGitLink file key
			ifM ((==) link <$> liftIO (readSymbolicLink file))
				( addlink
				, do
					liftIO $ removeFile file
					liftIO $ createSymbolicLink link file
					addlink
				)
		addlink = inRepo $ Git.Command.run "add"
			[Params "--force --", File file]

onDel :: FilePath -> Annex ()
onDel file = liftIO $ print $ "del " ++ file

{- A directory has been deleted, so tell git to remove anything that
   was inside it from its cache. -}
onDelDir :: FilePath -> Annex ()
onDelDir dir = inRepo $ Git.Command.run "rm"
	[Params "--quiet -r --cached --", File dir]

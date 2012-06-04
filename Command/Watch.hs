{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Watch where

import Common.Annex
import Command
import qualified Annex
import CmdLine
import Utility.Inotify
import Control.Exception as E
import qualified Command.Add as Add
import qualified Git.Command

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

onAdd :: FilePath -> Annex ()
onAdd file = void $ doCommand $ do
	showStart "add" file
	next $ Add.perform file

onAddSymlink :: FilePath -> Annex ()
onAddSymlink link = liftIO $ print $ "add symlink " ++ link

onDel :: FilePath -> Annex ()
onDel file = liftIO $ print $ "del " ++ file

onDelDir :: FilePath -> Annex ()
onDelDir dir = inRepo $ Git.Command.run "rm" [Params "--quiet -r", File dir]

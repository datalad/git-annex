{- git-annex webapp launcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.WebApp where

import Common.Annex
import Command
import Assistant
import Assistant.Common
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.Threads.WebApp
import Assistant.WebApp
import Utility.WebApp
import Utility.Daemon (checkDaemon, lockPidFile)
import Init
import qualified Git
import qualified Git.Config
import qualified Git.CurrentRepo
import qualified Annex
import Locations.UserConfig

import System.Posix.Directory
import Control.Concurrent
import Control.Concurrent.STM

def :: [Command]
def = [noCommit $ noRepo startNoRepo $ dontCheck repoExists $
        command "webapp" paramNothing seek "launch webapp"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = notBareRepo $ do
	ifM isInitialized ( go , liftIO startNoRepo )
	stop
	where
		go = do
			browser <- fromRepo webBrowser
			f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
			ifM (checkpid <&&> checkshim f)
				( liftIO $ openBrowser browser f 
				, startDaemon True True $ Just $
					const $ openBrowser browser
				)
		checkpid = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $ isJust <$> checkDaemon pidfile
		checkshim f = liftIO $ doesFileExist f

{- When run without a repo, see if there is an autoStartFile,
 - and if so, start the first available listed repository.
 - If not, it's our first time being run! -}
startNoRepo :: IO ()
startNoRepo = do
	autostartfile <- autoStartFile
	ifM (doesFileExist autostartfile) ( autoStart autostartfile , firstRun )

autoStart :: FilePath -> IO ()
autoStart autostartfile = do
	dirs <- lines <$> readFile autostartfile
	edirs <- filterM doesDirectoryExist dirs
	case edirs of
		[] -> firstRun -- what else can I do? Nothing works..
		(d:_) -> do
			changeWorkingDirectory d
			state <- Annex.new =<< Git.CurrentRepo.get
			void $ Annex.eval state $ doCommand start

{- Run the webapp without a repository, which prompts the user, makes one,
 - changes to it, starts the regular assistant, and redirects the
 - browser to its url.
 -
 - This is a very tricky dance -- The first webapp calls the signaler,
 - which signals the main thread when it's ok to continue by writing to a
 - MVar. The main thread starts the second webapp, and uses its callback
 - to write its url back to the MVar, from where the signaler retrieves it,
 - returning it to the first webapp, which does the redirect.
 -
 - Note that it's important that mainthread never terminates! Much
 - of this complication is due to needing to keep the mainthread running.
 -}
firstRun :: IO ()
firstRun = do
	dstatus <- atomically . newTMVar =<< newDaemonStatus
	scanremotes <- newScanRemoteMap
	transferqueue <- newTransferQueue
	transferslots <- newTransferSlots
	urlrenderer <- newUrlRenderer
	v <- newEmptyMVar
	let callback a = Just $ a v
	void $ runNamedThread dstatus $
		webAppThread Nothing dstatus scanremotes
			transferqueue transferslots urlrenderer
			(callback signaler) (callback mainthread)
	where
		signaler v = do
			putMVar v ""
			takeMVar v
		mainthread v _url htmlshim = do
			browser <- maybe Nothing webBrowser <$> Git.Config.global
			openBrowser browser htmlshim

			_wait <- takeMVar v

			state <- Annex.new =<< Git.CurrentRepo.get
			Annex.eval state $ do
				dummydaemonize
				startAssistant True id $ Just $ sendurlback v
		sendurlback v url _htmlshim = putMVar v url
		{- Set up the pid file in the new repo. -}
		dummydaemonize =
			liftIO . lockPidFile =<< fromRepo gitAnnexPidFile

openBrowser :: Maybe FilePath -> FilePath -> IO ()
openBrowser cmd htmlshim = go $ maybe runBrowser runCustomBrowser cmd
	where
		url = fileUrl htmlshim
		go a = unlessM (a url) $
			error $ "failed to start web browser on url " ++ url
		runCustomBrowser c u = boolSystem c [Param u]

{- web.browser is a generic git config setting for a web browser program -}
webBrowser :: Git.Repo -> Maybe FilePath
webBrowser = Git.Config.getMaybe "web.browser"

fileUrl :: FilePath -> String
fileUrl file = "file://" ++ file

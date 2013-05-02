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
import Assistant.NamedThread
import Assistant.Threads.WebApp
import Assistant.WebApp
import Assistant.Install
import Annex.Environment
import Utility.WebApp
import Utility.Daemon (checkDaemon)
import Init
import qualified Git
import qualified Git.Config
import qualified Git.CurrentRepo
import qualified Annex
import Config.Files
import qualified Option

import System.Posix.Directory
import Control.Concurrent
import Control.Concurrent.STM
import System.Process (env, std_out, std_err)
import Network.Socket (HostName)
import System.Environment

def :: [Command]
def = [ withOptions [listenOption] $
	noCommit $ noRepo startNoRepo $ dontCheck repoExists $ notBareRepo $
	command "webapp" paramNothing seek SectionCommon "launch webapp"]

listenOption :: Option
listenOption = Option.field [] "listen" paramAddress
	"accept connections to this address"

seek :: [CommandSeek]
seek = [withField listenOption return $ \listenhost ->
	withNothing $ start listenhost]

start :: Maybe HostName -> CommandStart
start = start' True

start' :: Bool -> Maybe HostName -> CommandStart
start' allowauto listenhost = do
	liftIO $ ensureInstalled
	ifM isInitialized ( go , auto )
	stop
  where
	go = do
		browser <- fromRepo webBrowser
		f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
		ifM (checkpid <&&> checkshim f)
			( if isJust listenhost
				then error "The assistant is already running, so --listen cannot be used."
				else liftIO $ openBrowser browser f Nothing Nothing
			, startDaemon True True listenhost $ Just $ 
				\origout origerr url htmlshim ->
					if isJust listenhost
						then maybe noop (`hPutStrLn` url) origout
						else openBrowser browser htmlshim url origout origerr
			)
	auto
		| allowauto = liftIO startNoRepo
		| otherwise = do
			d <- liftIO getCurrentDirectory
			error $ "no git repository in " ++ d
	checkpid = do
		pidfile <- fromRepo gitAnnexPidFile
		liftIO $ isJust <$> checkDaemon pidfile
	checkshim f = liftIO $ doesFileExist f

{- When run without a repo, start the first available listed repository in
 - the autostart file. If not, it's our first time being run! -}
startNoRepo :: IO ()
startNoRepo = do
	-- FIXME should be able to reuse regular getopt, but 
	-- it currently runs in the Annex monad.
	args <- getArgs
	let listenhost = headMaybe $ map (snd . separate (== '=')) $ 
		filter ("--listen=" `isPrefixOf`) args

	dirs <- liftIO $ filterM doesDirectoryExist =<< readAutoStartFile
	case dirs of
		[] -> firstRun listenhost
		(d:_) -> do
			changeWorkingDirectory d
			state <- Annex.new =<< Git.CurrentRepo.get
			void $ Annex.eval state $ doCommand $
				start' False listenhost

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
firstRun :: Maybe HostName -> IO ()
firstRun listenhost = do
	checkEnvironmentIO
	{- Without a repository, we cannot have an Annex monad, so cannot
	 - get a ThreadState. Using undefined is only safe because the
	 - webapp checks its noAnnex field before accessing the
	 - threadstate. -}
	let st = undefined
	{- Get a DaemonStatus without running in the Annex monad. -}
	dstatus <- atomically . newTMVar =<< newDaemonStatus
	d <- newAssistantData st dstatus
	urlrenderer <- newUrlRenderer
	v <- newEmptyMVar
	let callback a = Just $ a v
	runAssistant d $ do
		startNamedThread urlrenderer $
			webAppThread d urlrenderer True listenhost
				(callback signaler)
				(callback mainthread)
		waitNamedThreads
  where
	signaler v = do
		putMVar v ""
		takeMVar v
	mainthread v url htmlshim
		| isJust listenhost = do
			putStrLn url
			hFlush stdout
			go
		| otherwise = do
			browser <- maybe Nothing webBrowser <$> Git.Config.global
			openBrowser browser htmlshim url Nothing Nothing
			go
	  where
		go = do
			_wait <- takeMVar v
			state <- Annex.new =<< Git.CurrentRepo.get
			Annex.eval state $
				startDaemon True True listenhost $ Just $
					sendurlback v
	sendurlback v _origout _origerr url _htmlshim = putMVar v url

openBrowser :: Maybe FilePath -> FilePath -> String -> Maybe Handle -> Maybe Handle -> IO ()
openBrowser mcmd htmlshim realurl outh errh = do
	hPutStrLn (fromMaybe stdout outh) $ "Launching web browser on " ++ url
	hFlush stdout
	environ <- cleanEnvironment
	(_, _, _, pid) <- createProcess p
		{ env = environ
		, std_out = maybe Inherit UseHandle outh
		, std_err = maybe Inherit UseHandle errh
		}
	exitcode <- waitForProcess pid
	unless (exitcode == ExitSuccess) $
		hPutStrLn (fromMaybe stderr errh) "failed to start web browser"
  where
	p = case mcmd of
		Just cmd -> proc cmd [htmlshim]
		Nothing -> browserProc htmlshim url
#ifdef __ANDROID__
	{- Android does not support file:// urls, but neither is
	 - the security of the url in the process table important
	 - there, so just use the real url. -}
	url = realurl
#else
	url = fileUrl htmlshim
#endif

{- web.browser is a generic git config setting for a web browser program -}
webBrowser :: Git.Repo -> Maybe FilePath
webBrowser = Git.Config.getMaybe "web.browser"

fileUrl :: FilePath -> String
fileUrl file = "file://" ++ file

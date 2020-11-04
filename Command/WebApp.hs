{- git-annex webapp launcher
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.WebApp where

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
import Utility.UserInfo
import Annex.Init
import qualified Git
import Git.Types (fromConfigValue)
import qualified Git.Config
import qualified Git.CurrentRepo
import qualified Annex
import Config.Files.AutoStart
import Upgrade
import Annex.Version
import Utility.Android

import Control.Concurrent
import Control.Concurrent.STM

cmd :: Command
cmd = noCommit $ dontCheck repoExists $ notBareRepo $
	noRepo (startNoRepo <$$> optParser) $
		command "webapp" SectionCommon "launch webapp"
			paramNothing (seek <$$> optParser)

data WebAppOptions = WebAppOptions
	{ listenAddress :: Maybe String
	}

optParser :: CmdParamsDesc -> Parser WebAppOptions
optParser _ = WebAppOptions
	<$> optional (strOption
		( long "listen" <> metavar paramAddress
		<> help "accept connections to this address"
		))

seek :: WebAppOptions -> CommandSeek
seek = commandAction . start

start :: WebAppOptions -> CommandStart
start = start' True

start' :: Bool -> WebAppOptions -> CommandStart
start' allowauto o = do
	liftIO ensureInstalled
	ifM (isInitialized <&&> notHome)
		( maybe notinitialized (go <=< needsUpgrade) =<< getVersion
		, if allowauto
			then liftIO $ startNoRepo o
			else notinitialized
		)
	stop
  where
	go cannotrun = do
		browser <- fromRepo webBrowser
		f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
		listenAddress' <- if isJust (listenAddress o)
			then pure (listenAddress o)
			else annexListen <$> Annex.getGitConfig
		ifM (checkpid <&&> checkshim (fromRawFilePath f))
			( if isJust (listenAddress o)
				then giveup "The assistant is already running, so --listen cannot be used."
				else do
					url <- liftIO . readFile . fromRawFilePath
						=<< fromRepo gitAnnexUrlFile
					liftIO $ if isJust listenAddress'
						then putStrLn url
						else liftIO $ openBrowser browser (fromRawFilePath f) url Nothing Nothing
			, do
				startDaemon True True Nothing cannotrun listenAddress' $ Just $ 
					\origout origerr url htmlshim ->
						if isJust listenAddress'
							then maybe noop (`hPutStrLn` url) origout
							else openBrowser browser htmlshim url origout origerr
			)
	checkpid = do
		pidfile <- fromRepo gitAnnexPidFile
		liftIO $ isJust <$> checkDaemon (fromRawFilePath pidfile)
	checkshim f = liftIO $ doesFileExist f
	notinitialized = do
		g <- Annex.gitRepo
		liftIO $ cannotStartIn (Git.repoLocation g) "repository has not been initialized by git-annex"
		liftIO $ firstRun o

{- If HOME is a git repo, even if it's initialized for git-annex,
 - the user almost certianly does not want to run the assistant there. -}
notHome :: Annex Bool
notHome = do
	g <- Annex.gitRepo
	d <- liftIO $ absPath (Git.repoPath g)
	h <- liftIO $ absPath . toRawFilePath =<< myHomeDir
	return (d /= h)

{- When run without a repo, start the first available listed repository in
 - the autostart file. If none, it's our first time being run! -}
startNoRepo :: WebAppOptions -> IO ()
startNoRepo o = go =<< liftIO (filterM doesDirectoryExist =<< readAutoStartFile)
  where
	go [] = firstRun o
	go (d:ds) = do
		v <- tryNonAsync $ do
			setCurrentDirectory d
			Annex.new =<< Git.CurrentRepo.get
		case v of
			Left e -> do
				cannotStartIn d (show e)
				go ds
			Right state -> void $ Annex.eval state $ do
				whenM (fromRepo Git.repoIsLocalBare) $
					giveup $ d ++ " is a bare git repository, cannot run the webapp in it"
				callCommandAction $
					start' False o

cannotStartIn :: FilePath -> String -> IO ()
cannotStartIn d reason = warningIO $ "unable to start webapp in repository " ++ d ++ ": " ++ reason

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
firstRun :: WebAppOptions -> IO ()
firstRun o = do
	checkEnvironmentIO
	{- Without a repository, we cannot have an Annex monad, so cannot
	 - get a ThreadState. This is only safe because the
	 - webapp checks its noAnnex field before accessing the
	 - threadstate. -}
	let st = error "annex state not available"
	{- Get a DaemonStatus without running in the Annex monad. -}
	dstatus <- atomically . newTVar =<< newDaemonStatus
	d <- newAssistantData st dstatus
	urlrenderer <- newUrlRenderer
	v <- newEmptyMVar
	let callback a = Just $ a v
	runAssistant d $ do
		startNamedThread urlrenderer $
			webAppThread d urlrenderer True Nothing
				(callback signaler)
				(listenAddress o)
				(callback mainthread)
		waitNamedThreads
  where
	signaler v = do
		putMVar v ""
		takeMVar v
	mainthread v url htmlshim
		| isJust (listenAddress o)= do
			putStrLn url
			hFlush stdout
			go
		| otherwise = do
			browser <- maybe Nothing webBrowser
				<$> catchDefaultIO Nothing Git.Config.global
			openBrowser browser htmlshim url Nothing Nothing
			go
	  where
		go = do
			_wait <- takeMVar v
			state <- Annex.new =<< Git.CurrentRepo.get
			Annex.eval state $
				startDaemon True True Nothing Nothing (listenAddress o) $ Just $
					sendurlback v
	sendurlback v _origout _origerr url _htmlshim = putMVar v url

openBrowser :: Maybe FilePath -> FilePath -> String -> Maybe Handle -> Maybe Handle -> IO ()
openBrowser mcmd htmlshim realurl outh errh = do
	htmlshim' <- fromRawFilePath <$> absPath (toRawFilePath htmlshim)
	openBrowser' mcmd htmlshim' realurl outh errh

openBrowser' :: Maybe FilePath -> FilePath -> String -> Maybe Handle -> Maybe Handle -> IO ()
openBrowser' mcmd htmlshim realurl outh errh =
	ifM osAndroid
		{- Android does not support file:// urls well, but neither
		 - is the security of the url in the process table important
		 - there, so just use the real url. -}
		( runbrowser realurl
		, runbrowser (fileUrl htmlshim)
		)
  where
	runbrowser url = do
		let p = case mcmd of
			Just c -> proc c [url]
			Nothing -> 
#ifndef mingw32_HOST_OS
				browserProc url
#else
				{- Windows hack to avoid using the full path,
				 - which might contain spaces that cause problems
				 - for browserProc. -}
				(browserProc (takeFileName htmlshim))
					{ cwd = Just (takeDirectory htmlshim) } 
#endif
		hPutStrLn (fromMaybe stdout outh) $ "Launching web browser on " ++ url
		hFlush stdout
		environ <- cleanEnvironment
		let p' = p
			{ env = environ
			, std_out = maybe Inherit UseHandle outh
			, std_err = maybe Inherit UseHandle errh
			}
		withCreateProcess p' $ \_ _ _ pid -> do
			exitcode <- waitForProcess pid
			unless (exitcode == ExitSuccess) $
				hPutStrLn (fromMaybe stderr errh) "failed to start web browser"

{- web.browser is a generic git config setting for a web browser program -}
webBrowser :: Git.Repo -> Maybe FilePath
webBrowser = fmap fromConfigValue <$> Git.Config.getMaybe "web.browser"

fileUrl :: FilePath -> String
fileUrl file = "file://" ++ file

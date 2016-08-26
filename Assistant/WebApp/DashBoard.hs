{- git-annex assistant webapp dashboard
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.DashBoard where

import Assistant.WebApp.Common
import Assistant.WebApp.RepoList
import Assistant.WebApp.Notifications
import Assistant.TransferQueue
import Assistant.TransferSlots
import Assistant.DaemonStatus
import Utility.NotificationBroadcaster
import Types.Transfer
import Logs.Transfer
import Utility.Percentage
import Utility.DataUnits
import qualified Remote
import qualified Git

import qualified Text.Hamlet as Hamlet
import qualified Data.Map as M
import Control.Concurrent

{- A display of currently running and queued transfers. -}
transfersDisplay :: Widget
transfersDisplay = do
	current <- liftAssistant $ M.toList <$> getCurrentTransfers
	queued <- take 10 <$> liftAssistant getTransferQueue
	autoUpdate ident NotifierTransfersR (10 :: Int) (10 :: Int)
	let transfers = simplifyTransfers $ current ++ queued
	let transfersrunning = not $ null transfers
	scanrunning <- if transfersrunning
		then return False
		else liftAssistant $ transferScanRunning <$> getDaemonStatus
	$(widgetFile "dashboard/transfers")
  where
	ident = "transfers"
	isrunning info = not $
		transferPaused info || isNothing (startedTime info)

{- Simplifies a list of transfers, avoiding display of redundant
 - equivilant transfers. -}
simplifyTransfers :: [(Transfer, TransferInfo)] -> [(Transfer, TransferInfo)]
simplifyTransfers [] = []
simplifyTransfers (x:[]) = [x]
simplifyTransfers (v@(t1, _):r@((t2, _):l))
	| equivilantTransfer t1 t2 = simplifyTransfers (v:l)
	| otherwise = v : simplifyTransfers r

{- Called by client to get a display of currently in process transfers.
 -
 - Returns a div, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto the getDashboardR page.
 -}
getTransfersR :: NotificationId -> Handler Html
getTransfersR nid = do
	waitNotifier getTransferBroadcaster nid

	p <- widgetToPageContent transfersDisplay
	withUrlRenderer $ [hamlet|^{pageBody p}|]

{- The main dashboard. -}
dashboard :: Bool -> Widget
dashboard warnNoScript = do
	let repolist = repoListDisplay $
		mainRepoSelector { nudgeAddMore = True }
	let transferlist = transfersDisplay
	$(widgetFile "dashboard/main")

getDashboardR :: Handler Html
getDashboardR = ifM inFirstRun
	( redirect ConfigurationR
	, page "" (Just DashBoard) $ dashboard True
	)

{- Used to test if the webapp is running. -}
headDashboardR :: Handler ()
headDashboardR = noop

{- Same as DashboardR, except no autorefresh at all (and no noscript warning). -}
getNoScriptR :: Handler Html
getNoScriptR = page "" (Just DashBoard) $ dashboard False

{- Same as DashboardR, except with autorefreshing via meta refresh. -}
getNoScriptAutoR :: Handler Html
getNoScriptAutoR = page "" (Just DashBoard) $ do
	let delayseconds = 3 :: Int
	let this = NoScriptAutoR
	toWidgetHead $(Hamlet.hamletFile $ hamletTemplate "dashboard/metarefresh")
	dashboard False

{- The javascript code does a post. -}
postFileBrowserR :: Handler ()
postFileBrowserR = void openFileBrowser

{- Used by non-javascript browsers, where clicking on the link actually
 - opens this page, so we redirect back to the referrer. -}
getFileBrowserR :: Handler ()
getFileBrowserR = whenM openFileBrowser redirectBack

{- Opens the system file browser on the repo, or, as a fallback,
 - goes to a file:// url. Returns True if it's ok to redirect away
 - from the page (ie, the system file browser was opened). 
 -
 - Note that the command is opened using a different thread, to avoid
 - blocking the response to the browser on it. -}
openFileBrowser :: Handler Bool
openFileBrowser = do
	path <- liftIO . absPath =<< liftAnnex (fromRepo Git.repoPath)
#ifdef darwin_HOST_OS
	let cmd = "open"
	let p = proc cmd [path]
#else
#ifdef mingw32_HOST_OS
	{- Changing to the directory and then opening . works around
	 - spaces in directory name, etc. -}
	let cmd = "cmd"
	let p = (proc cmd ["/c start ."]) { cwd = Just path }
#else
	let cmd = "xdg-open"
	let p = proc cmd [path]
#endif
#endif
	ifM (liftIO $ inPath cmd)
		( do
			let run = void $ liftIO $ forkIO $ do
				(Nothing, Nothing, Nothing, pid) <- createProcess p
				void $ waitForProcess pid
			run
#ifdef mingw32_HOST_OS
			{- On windows, if the file browser is not
			 - already open, it comes up below the
			 - web browser when started. 
			 -
			 - Running it a second time brings it
			 - to the foreground.
			 -
			 - Seems to need a delay long enough for the file
			 - browser to be open in order to work. Here 1
			 - second. -}
			liftIO $ threadDelay 1000000
			run
#endif
			return True
		, do
			void $ redirect $ "file://" ++ path
			return False
		)

{- Transfer controls. The GET is done in noscript mode and redirects back
 - to the referring page. The POST is called by javascript. -}
getPauseTransferR :: Transfer -> Handler ()
getPauseTransferR = noscript postPauseTransferR
postPauseTransferR :: Transfer -> Handler ()
postPauseTransferR = liftAssistant . pauseTransfer
getStartTransferR :: Transfer -> Handler ()
getStartTransferR = noscript postStartTransferR
postStartTransferR :: Transfer -> Handler ()
postStartTransferR = liftAssistant . startTransfer
getCancelTransferR :: Transfer -> Handler ()
getCancelTransferR = noscript postCancelTransferR
postCancelTransferR :: Transfer -> Handler ()
postCancelTransferR = liftAssistant . cancelTransfer False

noscript :: (Transfer -> Handler ()) -> Transfer -> Handler ()
noscript a t = a t >> redirectBack

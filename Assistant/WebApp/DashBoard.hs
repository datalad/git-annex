{- git-annex assistant webapp dashboard
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.DashBoard where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.SideBar
import Assistant.WebApp.Notifications
import Assistant.WebApp.Configurators
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.TransferSlots
import qualified Assistant.Threads.Transferrer as Transferrer
import Utility.NotificationBroadcaster
import Utility.Yesod
import Logs.Transfer
import Utility.Percentage
import Utility.DataUnits
import Types.Key
import qualified Remote
import qualified Git
import Locations.UserConfig

import Yesod
import Text.Hamlet
import qualified Data.Map as M
import Control.Concurrent
import System.Posix.Signals (signalProcessGroup, sigTERM, sigKILL)
import System.Posix.Process (getProcessGroupIDOf)

{- A display of currently running and queued transfers.
 -
 - Or, if there have never been any this run, an intro display. -}
transfersDisplay :: Bool -> Widget
transfersDisplay warnNoScript = do
	webapp <- lift getYesod
	current <- lift $ M.toList <$> getCurrentTransfers
	queued <- liftIO $ getTransferQueue $ transferQueue webapp
	let ident = "transfers"
	autoUpdate ident NotifierTransfersR (10 :: Int) (10 :: Int)
	let transfers = simplifyTransfers $ current ++ queued
	if null transfers
		then ifM (lift $ showIntro <$> getWebAppState)
			( introDisplay ident
			, $(widgetFile "dashboard/transfers")
			)
		else $(widgetFile "dashboard/transfers")
	where
		isrunning info = not $
			transferPaused info || isNothing (startedTime info)

{- Simplifies a list of transfers, avoiding display of redundant
 - equivilant transfers. -}
simplifyTransfers :: [(Transfer, TransferInfo)] -> [(Transfer, TransferInfo)]
simplifyTransfers [] = []
simplifyTransfers (x:[]) = [x]
simplifyTransfers (v@(t1, _):r@((t2, _):l))
	| equivilantTransfer t1 t2 = simplifyTransfers (v:l)
	| otherwise = v : (simplifyTransfers r)

{- Called by client to get a display of currently in process transfers.
 -
 - Returns a div, which will be inserted into the calling page.
 -
 - Note that the head of the widget is not included, only its
 - body is. To get the widget head content, the widget is also 
 - inserted onto the getHomeR page.
 -}
getTransfersR :: NotificationId -> Handler RepHtml
getTransfersR nid = do
	waitNotifier transferNotifier nid

	page <- widgetToPageContent $ transfersDisplay False
	hamletToRepHtml $ [hamlet|^{pageBody page}|]

{- The main dashboard. -}
dashboard :: Bool -> Widget
dashboard warnNoScript = do
	sideBarDisplay
	let content = transfersDisplay warnNoScript
	$(widgetFile "dashboard/main")

getHomeR :: Handler RepHtml
getHomeR = ifM (inFirstRun)
	( redirect ConfigR
	, bootstrap (Just DashBoard) $ dashboard True
	)

{- Same as HomeR, except no autorefresh at all (and no noscript warning). -}
getNoScriptR :: Handler RepHtml
getNoScriptR = bootstrap (Just DashBoard) $ dashboard False

{- Same as HomeR, except with autorefreshing via meta refresh. -}
getNoScriptAutoR :: Handler RepHtml
getNoScriptAutoR = bootstrap (Just DashBoard) $ do
	let ident = NoScriptR
	let delayseconds = 3 :: Int
	let this = NoScriptAutoR
	toWidgetHead $(hamletFile $ hamletTemplate "dashboard/metarefresh")
	dashboard False

{- The javascript code does a post. -}
postFileBrowserR :: Handler ()
postFileBrowserR = void openFileBrowser

{- Used by non-javascript browsers, where clicking on the link actually
 - opens this page, so we redirect back to the referrer. -}
getFileBrowserR :: Handler ()
getFileBrowserR = whenM openFileBrowser $ redirectBack

redirectBack :: Handler ()
redirectBack = do
	clearUltDest
	setUltDestReferer
	redirectUltDest HomeR

{- Opens the system file browser on the repo, or, as a fallback,
 - goes to a file:// url. Returns True if it's ok to redirect away
 - from the page (ie, the system file browser was opened). 
 -
 - Note that the command is opened using a different thread, to avoid
 - blocking the response to the browser on it. -}
openFileBrowser :: Handler Bool
openFileBrowser = do
	path <- runAnnex (error "no configured repository") $
		fromRepo Git.repoPath
	ifM (liftIO $ inPath cmd <&&> inPath cmd)
		( do
			void $ liftIO $ forkIO $ void $
				boolSystem cmd [Param path]
			return True
		, do
			clearUltDest
			setUltDest $ "file://" ++ path
			void $ redirectUltDest HomeR
			return False
		)
	where
#if OSX
		cmd = "open"
#else
		cmd = "xdg-open"
#endif

{- Transfer controls. The GET is done in noscript mode and redirects back
 - to the referring page. The POST is called by javascript. -}
getPauseTransferR :: Transfer -> Handler ()
getPauseTransferR t = pauseTransfer t >> redirectBack
postPauseTransferR :: Transfer -> Handler ()
postPauseTransferR t = pauseTransfer t
getStartTransferR :: Transfer -> Handler ()
getStartTransferR t = startTransfer t >> redirectBack
postStartTransferR :: Transfer -> Handler ()
postStartTransferR t = startTransfer t
getCancelTransferR :: Transfer -> Handler ()
getCancelTransferR t = cancelTransfer False t >> redirectBack
postCancelTransferR :: Transfer -> Handler ()
postCancelTransferR t = cancelTransfer False t

pauseTransfer :: Transfer -> Handler ()
pauseTransfer = cancelTransfer True

cancelTransfer :: Bool -> Transfer-> Handler ()
cancelTransfer pause t = do
	webapp <- getYesod
	let dstatus = daemonStatus webapp
	m <- getCurrentTransfers
	liftIO $ do
		unless pause $
			{- remove queued transfer -}
			void $ dequeueTransfers (transferQueue webapp) dstatus $
				equivilantTransfer t
		{- stop running transfer -}
		maybe noop (stop dstatus) (M.lookup t m)
	where
		stop dstatus info = do
			{- When there's a thread associated with the
			 - transfer, it's signaled first, to avoid it
			 - displaying any alert about the transfer having
			 - failed when the transfer process is killed. -}
			maybe noop signalthread $ transferTid info
			maybe noop killproc $ transferPid info
			if pause
				then void $
					alterTransferInfo dstatus t $ \i -> i
						{ transferPaused = True }
				else void $
					removeTransfer dstatus t
		signalthread tid
			| pause = throwTo tid PauseTransfer
			| otherwise = killThread tid
		{- In order to stop helper processes like rsync,
		 - kill the whole process group of the process running the 
		 - transfer. -}
		killproc pid = do
			g <- getProcessGroupIDOf pid
			void $ tryIO $ signalProcessGroup sigTERM g
			threadDelay 50000 -- 0.05 second grace period
			void $ tryIO $ signalProcessGroup sigKILL g

startTransfer :: Transfer -> Handler ()
startTransfer t = do
	m <- getCurrentTransfers
	maybe startqueued go (M.lookup t m)
	where
		go info = maybe (start info) resume $ transferTid info
		startqueued = do
			webapp <- getYesod
			let dstatus = daemonStatus webapp
			let q = transferQueue webapp
			is <- liftIO $ map snd <$> getMatchingTransfers q dstatus (== t)
			maybe noop start $ headMaybe is
		resume tid = do
			webapp <- getYesod
			let dstatus = daemonStatus webapp
			liftIO $ do
				alterTransferInfo dstatus t $ \i -> i
					{ transferPaused = False }
				throwTo tid ResumeTransfer
		start info = do
			webapp <- getYesod
			let st = fromJust $ threadState webapp
			let dstatus = daemonStatus webapp
			let slots = transferSlots webapp
			liftIO $ inImmediateTransferSlot dstatus slots $ do
				program <- readProgramFile
				Transferrer.startTransfer st dstatus program t info

getCurrentTransfers :: Handler TransferMap
getCurrentTransfers = currentTransfers
	<$> (liftIO . getDaemonStatus =<< daemonStatus <$> getYesod)

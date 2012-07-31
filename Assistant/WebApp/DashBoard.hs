{- git-annex assistant webapp dashboard
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.DashBoard where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.SideBar
import Assistant.WebApp.Notifications
import Assistant.WebApp.Configurators
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Utility.NotificationBroadcaster
import Utility.Yesod
import Logs.Transfer
import Utility.Percentage
import Utility.DataUnits
import Types.Key
import qualified Remote

import Yesod
import Text.Hamlet
import qualified Data.Map as M

{- A display of currently running and queued transfers.
 -
 - Or, if there have never been any this run, an intro display. -}
transfersDisplay :: Bool -> Widget
transfersDisplay warnNoScript = do
	webapp <- lift getYesod
	current <- liftIO $ runThreadState (threadState webapp) $
		M.toList . currentTransfers
			<$> liftIO (getDaemonStatus $ daemonStatus webapp)
	queued <- liftIO $ getTransferQueue $ transferQueue webapp
	let ident = "transfers"
	autoUpdate ident NotifierTransfersR (10 :: Int) (10 :: Int)
	let transfers = current ++ queued
	if null transfers
		then ifM (lift $ showIntro <$> getWebAppState)
			( introDisplay ident
			, $(widgetFile "dashboard/transfers")
			)
		else $(widgetFile "dashboard/transfers")

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
getHomeR = defaultLayout $ dashboard True

{- Same as HomeR, except with autorefreshing via meta refresh. -}
getNoScriptAutoR :: Handler RepHtml
getNoScriptAutoR = defaultLayout $ do
	let ident = NoScriptR
	let delayseconds = 3 :: Int
	let this = NoScriptAutoR
	toWidgetHead $(hamletFile $ hamletTemplate "dashboard/metarefresh")
	dashboard False

{- Same as HomeR, except no autorefresh at all (and no noscript warning). -}
getNoScriptR :: Handler RepHtml
getNoScriptR = defaultLayout $
	dashboard False

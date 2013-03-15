{- git-annex assistant webapp core
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp where

import Assistant.WebApp.Types
import Assistant.Common hiding (liftAnnex)
import qualified Assistant.Monad as Assistant
import Utility.NotificationBroadcaster
import Utility.Yesod

import Yesod
import Data.Text (Text)
import Control.Concurrent

inFirstRun :: Handler Bool
inFirstRun = isNothing . relDir <$> getYesod

{- Runs an Annex action from the webapp.
 -
 - When the webapp is run outside a git-annex repository, the fallback
 - value is returned.
 -}
liftAnnexOr :: forall sub a. a -> Annex a -> GHandler sub WebApp a
liftAnnexOr fallback a = ifM (noAnnex <$> getYesod)
	( return fallback
	, liftAssistant $ Assistant.liftAnnex a
	)

liftAnnex :: forall sub a. Annex a -> GHandler sub WebApp a
liftAnnex = liftAnnexOr $ error "internal runAnnex"

liftAssistant :: forall sub a. (Assistant a) -> GHandler sub WebApp a
liftAssistant a = liftIO . flip runAssistant a =<< assistantData <$> getYesod

waitNotifier :: forall sub. (Assistant NotificationBroadcaster) -> NotificationId -> GHandler sub WebApp ()
waitNotifier getbroadcaster nid = liftAssistant $ do
	b <- getbroadcaster
	liftIO $ waitNotification $ notificationHandleFromId b nid

newNotifier :: forall sub. (Assistant NotificationBroadcaster) -> GHandler sub WebApp NotificationId
newNotifier getbroadcaster = liftAssistant $ do
	b <- getbroadcaster
	liftIO $ notificationHandleToId <$> newNotificationHandle b

{- Adds the auth parameter as a hidden field on a form. Must be put into
 - every form. -}
webAppFormAuthToken :: Widget
webAppFormAuthToken = do
	webapp <- lift getYesod
	[whamlet|<input type="hidden" name="auth" value="#{secretToken webapp}">|]

{- A button with an icon, and maybe label or tooltip, that can be
 - clicked to perform some action.
 - With javascript, clicking it POSTs the Route, and remains on the same
 - page.
 - With noscript, clicking it GETs the Route. -}
actionButton :: Route WebApp -> (Maybe String) -> (Maybe String) -> String -> String -> Widget
actionButton route label tooltip buttonclass iconclass = $(widgetFile "actionbutton")

type UrlRenderFunc = Route WebApp -> [(Text, Text)] -> Text
type UrlRenderer = MVar (UrlRenderFunc)

newUrlRenderer :: IO UrlRenderer
newUrlRenderer = newEmptyMVar

setUrlRenderer :: UrlRenderer -> (UrlRenderFunc) -> IO ()
setUrlRenderer = putMVar

{- Blocks until the webapp is running and has called setUrlRenderer. -}
renderUrl :: UrlRenderer -> Route WebApp -> [(Text, Text)] -> IO Text
renderUrl urlrenderer route params = do
	r <- readMVar urlrenderer
	return $ r route params

{- Redirects back to the referring page, or if there's none, DashboardR -}
redirectBack :: Handler ()
redirectBack = do
	clearUltDest
	setUltDestReferer
	redirectUltDest DashboardR

controlMenu :: Widget
controlMenu = $(widgetFile "controlmenu")

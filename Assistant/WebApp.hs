{- git-annex assistant webapp core
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp where

import Assistant.WebApp.Types
import Assistant.Common
import Utility.NotificationBroadcaster
import Utility.Yesod
import Utility.AuthToken

import Data.Text (Text)
import Control.Concurrent
import qualified Network.Wai as W
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

waitNotifier :: Assistant NotificationBroadcaster -> NotificationId -> Handler ()
waitNotifier getbroadcaster nid = liftAssistant $ do
	b <- getbroadcaster
	liftIO $ waitNotification $ notificationHandleFromId b nid

newNotifier :: Assistant NotificationBroadcaster -> Handler NotificationId
newNotifier getbroadcaster = liftAssistant $ do
	b <- getbroadcaster
	liftIO $ notificationHandleToId <$> newNotificationHandle True b

{- Adds the auth parameter as a hidden field on a form. Must be put into
 - every form. -}
webAppFormAuthToken :: Widget
webAppFormAuthToken = do
	webapp <- liftH getYesod
	[whamlet|<input type="hidden" name="auth" value="#{fromAuthToken (authToken webapp)}">|]

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

inFirstRun :: Handler Bool
inFirstRun = isNothing . relDir <$> getYesod

{- Blocks until the webapp is running and has called setUrlRenderer. -}
renderUrl :: UrlRenderer -> Route WebApp -> [(Text, Text)] -> IO Text
renderUrl urlrenderer route params = do
	r <- readMVar urlrenderer
	return $ r route params

{- Redirects back to the referring page, or if there's none, DashboardR -}
redirectBack :: Handler ()
redirectBack = do
	mr <- lookup "referer" . W.requestHeaders <$> waiRequest
	case mr of
		Nothing -> redirect DashboardR
		Just r -> redirect $ T.pack $ S8.unpack r

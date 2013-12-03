{- git-annex assistant webapp page display
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes, CPP #-}

module Assistant.WebApp.Page where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod

import qualified Text.Hamlet as Hamlet
import Data.Text (Text)

data NavBarItem = DashBoard | Configuration | About
	deriving (Eq, Ord, Enum, Bounded)

navBarName :: NavBarItem -> Text
navBarName DashBoard = "Dashboard"
navBarName Configuration = "Configuration"
navBarName About = "About"

navBarRoute :: NavBarItem -> Route WebApp
navBarRoute DashBoard = DashboardR
navBarRoute Configuration = ConfigurationR
navBarRoute About = AboutR

defaultNavBar :: [NavBarItem]
defaultNavBar = [minBound .. maxBound]

firstRunNavBar :: [NavBarItem]
firstRunNavBar = [Configuration, About]

selectNavBar :: Handler [NavBarItem]
selectNavBar = ifM inFirstRun (return firstRunNavBar, return defaultNavBar)

{- A standard page of the webapp, with a title, a sidebar, and that may
 - be highlighted on the navbar. -}
page :: Hamlet.Html -> Maybe NavBarItem -> Widget -> Handler Html
page title navbaritem content = customPage navbaritem $ do
	setTitle title
	content
	sideBarDisplay

{- A custom page, with no title or sidebar set. -}
customPage :: Maybe NavBarItem -> Widget -> Handler Html
customPage = customPage' True

customPage' :: Bool -> Maybe NavBarItem -> Widget -> Handler Html
customPage' with_longpolling navbaritem content = do
	webapp <- getYesod
	case cannotRun webapp of
		Nothing -> do
			navbar <- map navdetails <$> selectNavBar
			pageinfo <- widgetToPageContent $ do
				addStylesheet $ StaticR css_bootstrap_css
				addStylesheet $ StaticR css_bootstrap_responsive_css
				addScript $ StaticR jquery_full_js
				addScript $ StaticR js_bootstrap_dropdown_js
				addScript $ StaticR js_bootstrap_modal_js
				addScript $ StaticR js_bootstrap_collapse_js
				when with_longpolling $
					addScript $ StaticR longpolling_js
				$(widgetFile "page")
			giveUrlRenderer $(Hamlet.hamletFile $ hamletTemplate "bootstrap")
		Just msg -> error msg
  where
	navdetails i = (navBarName i, navBarRoute i, Just i == navbaritem)

hasFileBrowser :: Bool
#ifdef ANDROID_SPLICES
hasFileBrowser = False
#else
hasFileBrowser = True
#endif

controlMenu :: Widget
controlMenu = $(widgetFile "controlmenu")

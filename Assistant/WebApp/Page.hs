{- git-annex assistant webapp page display
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Page where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod

import Yesod
import Text.Hamlet
import Data.Text (Text)

data NavBarItem = DashBoard | Configuration | About
	deriving (Eq)

navBarName :: NavBarItem -> Text
navBarName DashBoard = "Dashboard"
navBarName Configuration = "Configuration"
navBarName About = "About"

navBarRoute :: NavBarItem -> Route WebApp
navBarRoute DashBoard = HomeR
navBarRoute Configuration = ConfigurationR
navBarRoute About = AboutR

defaultNavBar :: [NavBarItem]
defaultNavBar = [DashBoard, Configuration, About]

firstRunNavBar :: [NavBarItem]
firstRunNavBar = [Configuration, About]

selectNavBar :: Handler [NavBarItem]
selectNavBar = ifM (inFirstRun) (return firstRunNavBar, return defaultNavBar)

{- A standard page of the webapp, with a title, a sidebar, and that may
 - be highlighted on the navbar. -}
page :: Html -> Maybe NavBarItem -> Widget -> Handler RepHtml
page title navbaritem content = customPage navbaritem $ do
	setTitle title
	sideBarDisplay
	content

{- A custom page, with no title or sidebar set. -}
customPage :: Maybe NavBarItem -> Widget -> Handler RepHtml
customPage navbaritem content = do
	webapp <- getYesod
	navbar <- map navdetails <$> selectNavBar
	pageinfo <- widgetToPageContent $ do
		addStylesheet $ StaticR css_bootstrap_css
		addStylesheet $ StaticR css_bootstrap_responsive_css
		addScript $ StaticR jquery_full_js
		addScript $ StaticR js_bootstrap_dropdown_js
		addScript $ StaticR js_bootstrap_modal_js
		addScript $ StaticR js_bootstrap_collapse_js
		$(widgetFile "page")
	hamletToRepHtml $(hamletFile $ hamletTemplate "bootstrap")
  where
	navdetails i = (navBarName i, navBarRoute i, Just i == navbaritem)

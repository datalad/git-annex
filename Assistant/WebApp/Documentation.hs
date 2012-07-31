{- git-annex assistant webapp documentation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Documentation where

import Assistant.WebApp
import Assistant.WebApp.SideBar
import Utility.Yesod

import Yesod

getAboutR :: Handler RepHtml
getAboutR = bootstrap (Just About) $ do
	sideBarDisplay
	setTitle "About git-annex"
	$(widgetFile "documentation/about")

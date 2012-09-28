{- git-annex assistant webapp documentation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Documentation where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Assistant.Install (standaloneAppBase)
import Utility.Yesod
import Build.SysConfig (packageversion)

import Yesod

{- The full license info may be included in a file on disk that can
 - be read in and displayed. -}
licenseFile :: IO (Maybe FilePath)
licenseFile = do
	base <- standaloneAppBase
	return $ (</> "LICENSE") <$> base

getAboutR :: Handler RepHtml
getAboutR = bootstrap (Just About) $ do
	sideBarDisplay
	setTitle "About git-annex"
	builtinlicense <- isJust <$> liftIO licenseFile
	$(widgetFile "documentation/about")

getLicenseR :: Handler RepHtml
getLicenseR = do
	v <- liftIO licenseFile
	case v of
		Nothing -> redirect AboutR
		Just f -> bootstrap (Just About) $ do
			-- no sidebar, just pages of legalese..
			setTitle "License"
			license <- liftIO $ readFile f
			$(widgetFile "documentation/license")

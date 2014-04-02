{- git-annex assistant webapp documentation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Documentation where

import Assistant.WebApp.Common
import Assistant.Install (standaloneAppBase)
import Build.SysConfig (packageversion)
import BuildFlags

{- The full license info may be included in a file on disk that can
 - be read in and displayed. -}
licenseFile :: IO (Maybe FilePath)
licenseFile = do
	base <- standaloneAppBase
	return $ (</> "LICENSE") <$> base

getAboutR :: Handler Html
getAboutR = page "About git-annex" (Just About) $ do
	builtinlicense <- isJust <$> liftIO licenseFile
	$(widgetFile "documentation/about")

getLicenseR :: Handler Html
getLicenseR = do
	v <- liftIO licenseFile
	case v of
		Nothing -> redirect AboutR
		Just f -> customPage (Just About) $ do
			-- no sidebar, just pages of legalese..
			setTitle "License"
			license <- liftIO $ readFile f
			$(widgetFile "documentation/license")

getRepoGroupR :: Handler Html
getRepoGroupR = page "About repository groups" (Just About) $
	$(widgetFile "documentation/repogroup")
